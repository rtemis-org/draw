// rtemis-diagram htmlwidget binding.
//
// Renders Mermaid diagram text with Pele (window.pele, lib/pele/pele.global.js)
// as SVG. Colors and fonts are Pele CSS variables set on the widget element
// from the resolved draw theme (lib/draw/diagram_theme.js), so a light/dark
// switch recolors the diagram without rendering it again.
// spec: draw/mermaid-diagrams#sizing-and-scrolling
//
// By default the diagram renders at its natural size inside a scrolling
// element, so labels keep their font size however long or wide the diagram
// is. `style.fit` instead shrinks the diagram to the element width, with
// Pele's pinch and Ctrl/Cmd-wheel zoom.

HTMLWidgets.widget({
  name: "rtemis-diagram",
  type: "output",

  factory: (el, width, height) => {
    let current = null;
    let renderedDark = null;
    let mounted = null;

    el.style.overflow = "auto";
    el.style.boxSizing = "border-box";
    const stage = document.createElement("div");
    el.appendChild(stage);

    const activeTheme = (x) => {
      renderedDark = window.RtemisThemeWatch.isDark();
      return x.autoTheme && renderedDark ? x.themeDark : x.theme;
    };

    // Set the Pele variables for the active theme on the widget element, and
    // paint the element background to match the diagram background.
    const applyTheme = (x) => {
      const theme = activeTheme(x);
      const values = window.RtemisDiagramTheme.vars(theme, x.style);
      for (let i = el.style.length - 1; i >= 0; i--) {
        const name = el.style[i];
        if (name.startsWith("--pele-")) el.style.removeProperty(name);
      }
      Object.entries(values).forEach(([name, value]) => {
        el.style.setProperty("--pele-" + name, value);
      });
      el.style.backgroundColor = values.bg || "";
      el.style.color = values.fg || "";
      return theme;
    };

    // Pele error messages contain diagram source, so they are inserted as
    // text, never as markup.
    const showError = (error) => {
      stage.textContent = "";
      stage.style.cssText = "";
      const pre = document.createElement("pre");
      pre.style.cssText =
        "margin:0;padding:12px;white-space:pre-wrap;font-size:12px;" +
        "color:var(--pele-error,#e45756);";
      pre.textContent = "Mermaid diagram error: " +
        (error && error.message ? error.message : String(error));
      stage.appendChild(pre);
    };

    // Documents (knitr, Quarto) size the widget to the diagram when R left
    // the height unset; the page then scrolls vertically. A standalone page,
    // which the RStudio and VS Code Viewer panes display, holds the widget in
    // #htmlwidget_container and fills the window; it and Shiny keep the element
    // size and scroll inside it.
    const fitHeight = (x) => {
      const standalone = !!document.getElementById("htmlwidget_container");
      if (x.autoHeight && !standalone && !HTMLWidgets.shinyMode) {
        el.style.height = "auto";
      }
    };

    const renderDiagram = (x) => {
      current = x;
      if (mounted) {
        mounted.destroy();
        mounted = null;
      }
      stage.textContent = "";
      if (!window.pele) {
        showError(new Error("the Pele library failed to load (window.pele is missing)."));
        return;
      }
      const theme = applyTheme(x);
      const options = window.RtemisDiagramTheme.options(
        x.style, theme, el.id ? el.id + "-" : null
      );
      const style = x.style || {};
      const colorNodes = (svg) => window.RtemisDiagramTheme.colorNodes(
        svg, style.nodeColor, style.fillAlpha
      );
      if (style.fit) {
        stage.style.cssText = "width:100%;";
        mounted = window.pele.mount(stage, x.source, {
          ...options,
          zoom: "auto",
          onError: (error) => console.error("rtemis-diagram:", error),
          // mount() inserts each render through `sanitize`, which colors the
          // named nodes before insertion.
          sanitize: (svg) => {
            const template = document.createElement("template");
            template.innerHTML = colorNodes(svg);
            return template.content;
          },
        });
      } else {
        // max-content keeps the natural width; auto margins center a diagram
        // narrower than the element and collapse to zero for a wider one.
        stage.style.cssText = "width:max-content;margin:0 auto;";
        try {
          const result = window.pele.render(x.source, {...options, responsive: false});
          stage.innerHTML = colorNodes(result.svg);
        } catch (error) {
          showError(error);
        }
      }
      fitHeight(x);
    };

    // Pele sizes nodes from measured label widths, so a web font that is
    // still loading is measured again once it is ready.
    const whenFontsReady = (x) => {
      if (document.fonts && document.fonts.status === "loading") {
        document.fonts.ready.then(() => {
          if (current === x) renderDiagram(x);
        });
      }
    };

    window.RtemisThemeWatch.watch(el, () => {
      if (!current || !current.autoTheme) return;
      if (window.RtemisThemeWatch.isDark() === renderedDark) return;
      applyTheme(current);
    }, () => {
      if (mounted) mounted.destroy();
      mounted = null;
    });

    return {
      renderValue: (x) => {
        renderDiagram(x);
        whenFontsReady(x);
      },
      // mount() follows the element width itself; a natural-size diagram
      // keeps its size and the element scrolls.
      resize: (newWidth, newHeight) => {},
    };
  },
});
