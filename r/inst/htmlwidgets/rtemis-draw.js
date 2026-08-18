HTMLWidgets.widget({
  name: "rtemis-draw",
  type: "output",

  factory: (el, width, height) => {
    let currentWidth = width;
    let currentHeight = height;
    let chart = null;
    let currentPayload = null;
    let renderedDark = null;
    let injectedVisualMapColor = false;

    // Detect dark mode from VS Code, RStudio, Quarto, or browser preference
    const isDarkMode = () => {
      const body = document.body;
      // VS Code webview
      if (body.classList.contains("vscode-dark") ||
          body.classList.contains("vscode-high-contrast")) {
        return true;
      }
      if (body.classList.contains("vscode-light")) {
        return false;
      }
      // RStudio
      if (body.classList.contains("rstudio-themes-dark-menus")) {
        return true;
      }
      // Quarto: `toggleBodyColorMode()` in the emitted page sets exactly one of
      // these on <body>, from the reader's saved choice. It wins over the media
      // query because the reader picked it explicitly.
      if (body.classList.contains("quarto-dark")) {
        return true;
      }
      if (body.classList.contains("quarto-light")) {
        return false;
      }
      // Browser / system preference
      if (window.matchMedia) {
        return window.matchMedia("(prefers-color-scheme: dark)").matches;
      }
      return false;
    };

    // For square-cell heatmaps: compute the required height given a container
    // width, so that grid cells are perfectly square.
    const squareCellHeight = (x, containerWidth) => {
      const gridWidth = containerWidth - x.leftPx - x.rightPx;
      const cellPx = gridWidth / x.nCols;
      return Math.round(x.nRows * cellPx + x.topPx + x.botPx);
    };

    // For `aspect` charts: size the plotting grid from the measured container
    // width so that the grid keeps a fixed height:width ratio.
    //
    // This is the one thing R cannot do. Equal axis scaling (one data unit =
    // the same number of pixels on both axes) is a constraint on the *pixel*
    // geometry of the grid, ECharts has no option for it, and R does not know
    // how wide the container will be. So R sends the ratio it needs and the
    // padding the axis labels require, and the browser solves for the box.
    //
    // `widthPx` is the preferred grid width: the box stays that size wherever
    // there is room and shrinks to fit only when there is not, so a wide page
    // centers a fixed-size figure rather than inflating it to fill the column.
    const aspectBox = (a, containerWidth) => {
      const avail = containerWidth - a.leftPx - a.rightPx;
      const preferred = a.widthPx || avail;
      const gridWidth = Math.max(1, Math.min(preferred, avail));
      const gridHeight = gridWidth * a.ratio;
      return {
        gridWidth,
        gridHeight,
        height: Math.round(gridHeight + a.topPx + a.botPx)
      };
    };

    // ECharts accepts `visualMap` as a single component or an array of them.
    // Both post-processing steps below have to reach every one, so they iterate
    // this rather than assuming the single-component shape.
    const visualMaps = (option) => {
      const vm = option?.visualMap;
      if (!vm) return [];
      return Array.isArray(vm) ? vm : [vm];
    };

    const renderChart = (x) => {
      if (chart) {
        chart.dispose();
        chart = null;
      }

      let themeName = null;
      let themeObj = null;
      const dark = isDarkMode();

      if (x.autoTheme) {
        // Auto-detect: pick light or dark theme
        themeObj = dark ? x.themeDark : x.theme;
      } else if (x.theme) {
        themeObj = x.theme;
      }
      renderedDark = dark;

      if (themeObj) {
        // Propagate the global theme text color to visualMap labels.
        // ECharts does not automatically inherit global textStyle into
        // visualMap.textStyle, so we inject it here before registering.
        // `injectedVisualMapColor` distinguishes a color we wrote on an earlier
        // render (which must be replaced when the theme flips) from one the
        // caller set in R (which must not be touched).
        const fgColor = themeObj.textStyle?.color;
        if (fgColor) {
          visualMaps(x.option).forEach((vm) => {
            if (!vm.textStyle) vm.textStyle = {};
            if (!vm.textStyle.color || injectedVisualMapColor) {
              vm.textStyle.color = fgColor;
              injectedVisualMapColor = true;
            }
          });
        }

        echarts.registerTheme("custom_theme", themeObj);
        themeName = "custom_theme";
      }

      // Sync the widget container (and its immediate parent) background to the
      // chart background so there is no white gutter around the canvas in
      // dark-themed viewers, without mutating global body styles.
      const bgColor =
        themeObj?.backgroundColor ||
        x.option?.backgroundColor ||
        null;
      if (bgColor) {
        el.style.backgroundColor = bgColor;
        if (el.parentElement) {
          el.parentElement.style.backgroundColor = bgColor;
        }
      }

      // Substitute the theme-matched heatmap colour palette when R has
      // pre-computed both light and dark variants.  The dark palette places
      // the theme background colour exactly at 0 for diverging scales.
      if (x.colorLight || x.colorDark) {
        const hmColors = x.colorDark
          ? (dark ? x.colorDark : x.colorLight)
          : x.colorLight;
        if (hmColors) {
          visualMaps(x.option).forEach((vm) => {
            if (!vm.inRange) vm.inRange = {};
            vm.inRange.color = hmColors;
          });
        }
      }

      chart = echarts.init(el, themeName, {
        renderer: x.renderer || "canvas",
        width: currentWidth,
        height: currentHeight
      });

      chart.setOption(x.option, true);

      // Double-click resets any dataZoom (e.g. the gantt's inside zoom) back to
      // the full view -- a familiar gesture, in addition to the toolbox reset.
      // No-op when the chart has no dataZoom, so it's safe for every chart type.
      // Re-apply the option's own dataZoom components with start/end = 0/100:
      // setOption merges by index onto exactly those components, resetting every
      // axis (the toolbox's internal dataZoom shift dispatchAction indices, so
      // index-based dispatch would miss an axis). Legend selection is untouched.
      const zoomDefs = x.option?.dataZoom;
      if (zoomDefs?.length) {
        chart.getZr().on("dblclick", () => {
          chart.setOption({
            dataZoom: zoomDefs.map((d) =>
              Object.assign({}, d, { start: 0, end: 100 })
            )
          });
        });
      }
    };

    // Re-render when the effective color scheme changes. The guard compares
    // against the scheme actually rendered, so unrelated body-class churn --
    // which the observer below sees a lot of -- does not cost a re-render.
    const onThemeChange = () => {
      // The observer below is attached to <body>, which outlives this widget:
      // once the container is gone (Shiny re-rendering dynamic UI, a tab being
      // torn down) the listener would keep firing on a detached element, and
      // one leaks per widget ever rendered. Detaching here is the teardown
      // hook htmlwidgets does not give us.
      if (!el.isConnected) {
        stopWatchingTheme();
        return;
      }
      if (!currentPayload?.autoTheme) return;
      if (isDarkMode() === renderedDark) return;
      renderChart(currentPayload);
    };

    let stopWatchingTheme = () => {};

    // Two independent triggers, one guard. The media query catches an OS or
    // browser preference change; the observer catches an in-page toggle, since
    // Quarto's light/dark switch only rewrites `body.class` -- it fires no event
    // and does not touch the media query, so without it the page re-themes and
    // the charts do not.
    {
      const teardown = [];

      if (window.matchMedia) {
        const mq = window.matchMedia("(prefers-color-scheme: dark)");
        if (mq.addEventListener) {
          mq.addEventListener("change", onThemeChange);
          teardown.push(() => mq.removeEventListener("change", onThemeChange));
        } else if (mq.addListener) {
          mq.addListener(onThemeChange);
          teardown.push(() => mq.removeListener(onThemeChange));
        }
      }

      if (window.MutationObserver && document.body) {
        const observer = new MutationObserver(onThemeChange);
        observer.observe(document.body, {
          attributes: true,
          attributeFilter: ["class"]
        });
        teardown.push(() => observer.disconnect());
      }

      stopWatchingTheme = () => {
        teardown.forEach((off) => off());
        teardown.length = 0;
      };
    }

    // Resolve the grid box and container height for an `aspect` chart at the
    // current container width, writing the result into the option so that the
    // next setOption() uses it. Returns the container height, or null when the
    // payload declares no aspect.
    const applyAspect = (x, containerWidth) => {
      if (!x?.aspect || !x.option?.grid) return null;
      // A multi-grid option is out of scope: aspect describes one plotting box.
      if (Array.isArray(x.option.grid)) return null;
      const box = aspectBox(x.aspect, containerWidth);
      x.option.grid.width = box.gridWidth;
      x.option.grid.height = box.gridHeight;
      el.style.height = `${box.height}px`;
      return box.height;
    };

    return {
      renderValue: (x) => {
        currentPayload = x;

        // Square-cell heatmaps: enforce the correct height by deriving it
        // from the actual container width and the layout margins passed from R.
        // This overrides whatever height htmlwidgets allocated for the container,
        // ensuring cells are always square regardless of viewer window dimensions.
        if (x.squareCells) {
          const newHeight = squareCellHeight(x, currentWidth);
          el.style.height = `${newHeight}px`;
          currentHeight = newHeight;
        }

        const aspectHeight = applyAspect(x, currentWidth);
        if (aspectHeight !== null) currentHeight = aspectHeight;

        renderChart(x);
      },

      resize: (width, height) => {
        currentWidth = width;
        currentHeight = height;

        if (currentPayload?.squareCells) {
          // Recompute height to keep cells square at the new width
          const newHeight = squareCellHeight(currentPayload, width);
          el.style.height = `${newHeight}px`;
          currentHeight = newHeight;
          if (chart) chart.resize({ width, height: newHeight });
          return;
        }

        // Re-solve the grid box at the new width, then push it onto the live
        // chart: resizing alone would stretch the grid and break the scale.
        // `applyAspect` returns null for a payload it does not solve, which
        // falls through to the plain resize rather than handing ECharts a null
        // height.
        const aspectHeight = applyAspect(currentPayload, width);
        if (aspectHeight !== null) {
          currentHeight = aspectHeight;
          if (chart) {
            chart.resize({ width, height: aspectHeight });
            chart.setOption({ grid: currentPayload.option.grid });
          }
        } else if (chart) {
          chart.resize({ width, height });
        }
      },

      getChart: () => chart
    };
  }
});
