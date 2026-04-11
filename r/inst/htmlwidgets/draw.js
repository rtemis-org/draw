HTMLWidgets.widget({
  name: "draw",
  type: "output",

  factory: function(el, width, height) {
    var currentWidth = width;
    var currentHeight = height;
    var chart = null;

    return {
      renderValue: function(x) {
        // Dispose previous instance if re-rendering
        if (chart) {
          chart.dispose();
          chart = null;
        }

        // Register theme if provided
        var themeName = null;
        if (x.theme) {
          echarts.registerTheme("custom_theme", x.theme);
          themeName = "custom_theme";
        }

        // Single initialization with the correct theme
        chart = echarts.init(el, themeName, {
          renderer: x.renderer || "canvas",
          width: currentWidth,
          height: currentHeight
        });

        chart.setOption(x.option, true);
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
