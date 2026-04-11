HTMLWidgets.widget({
  name: "draw",
  type: "output",

  factory: function(el, width, height) {
    var currentWidth = width;
    var currentHeight = height;
    var chart = null;
    var currentPayload = null;

    // Detect dark mode from VS Code, RStudio, or browser preference
    function isDarkMode() {
      var body = document.body;
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
    }

    function renderChart(x) {
      if (chart) {
        chart.dispose();
        chart = null;
      }

      var themeName = null;
      var themeObj = null;

      if (x.autoTheme) {
        // Auto-detect: pick light or dark theme
        themeObj = isDarkMode() ? x.themeDark : x.theme;
      } else if (x.theme) {
        themeObj = x.theme;
      }

      if (themeObj) {
        echarts.registerTheme("custom_theme", themeObj);
        themeName = "custom_theme";
      }

      chart = echarts.init(el, themeName, {
        renderer: x.renderer || "canvas",
        width: currentWidth,
        height: currentHeight
      });

      chart.setOption(x.option, true);
    }

    // Listen for system color scheme changes
    if (window.matchMedia) {
      var mq = window.matchMedia("(prefers-color-scheme: dark)");
      var onChange = function() {
        if (currentPayload && currentPayload.autoTheme) {
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
      renderValue: function(x) {
        currentPayload = x;
        renderChart(x);
      },

      resize: function(width, height) {
        currentWidth = width;
        currentHeight = height;
        if (chart) {
          chart.resize({
            width: width,
            height: height
          });
        }
      },

      getChart: function() {
        return chart;
      }
    };
  }
});
