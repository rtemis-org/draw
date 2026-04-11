HTMLWidgets.widget({
  name: "draw",
  type: "output",

  factory: function(el, width, height) {
    var currentWidth = width;
    var currentHeight = height;
    var chart = echarts.init(el, null, {
      renderer: "canvas",
      width: currentWidth,
      height: currentHeight
    });

    return {
      renderValue: function(x) {
        // Register and apply theme if provided
        if (x.theme) {
          echarts.registerTheme("custom_theme", x.theme);
          chart.dispose();
          chart = echarts.init(el, "custom_theme", {
            renderer: x.renderer || "canvas",
            width: currentWidth,
            height: currentHeight
          });
        }

        // Set the option
        chart.setOption(x.option, true);
      },

      resize: function(width, height) {
        currentWidth = width;
        currentHeight = height;
        chart.resize({
          width: width,
          height: height
        });
      },

      getChart: function() {
        return chart;
      }
    };
  }
});
