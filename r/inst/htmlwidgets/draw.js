HTMLWidgets.widget({
  name: "draw",
  type: "output",

  factory: (el, width, height) => {
    let currentWidth = width;
    let currentHeight = height;
    let chart = null;
    let currentPayload = null;

    // Detect dark mode from VS Code, RStudio, or browser preference
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

    const renderChart = (x) => {
      if (chart) {
        chart.dispose();
        chart = null;
      }

      let themeName = null;
      let themeObj = null;

      if (x.autoTheme) {
        // Auto-detect: pick light or dark theme
        themeObj = isDarkMode() ? x.themeDark : x.theme;
      } else if (x.theme) {
        themeObj = x.theme;
      }

      if (themeObj) {
        // Propagate the global theme text color to visualMap labels.
        // ECharts does not automatically inherit global textStyle into
        // visualMap.textStyle, so we inject it here before registering.
        const fgColor = themeObj.textStyle?.color;
        if (fgColor && x.option.visualMap) {
          if (!x.option.visualMap.textStyle) {
            x.option.visualMap.textStyle = {};
          }
          if (!x.option.visualMap.textStyle.color) {
            x.option.visualMap.textStyle.color = fgColor;
          }
        }

        echarts.registerTheme("custom_theme", themeObj);
        themeName = "custom_theme";
      }

      chart = echarts.init(el, themeName, {
        renderer: x.renderer || "canvas",
        width: currentWidth,
        height: currentHeight
      });

      chart.setOption(x.option, true);
    };

    // Listen for system color scheme changes
    if (window.matchMedia) {
      const mq = window.matchMedia("(prefers-color-scheme: dark)");
      const onChange = () => {
        if (currentPayload?.autoTheme) {
          renderChart(currentPayload);
        }
      };
      if (mq.addEventListener) {
        mq.addEventListener("change", onChange);
      } else if (mq.addListener) {
        mq.addListener(onChange);
      }
    }

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
        } else {
          if (chart) chart.resize({ width, height });
        }
      },

      getChart: () => chart
    };
  }
});
