var defaultOptions = {
  showThemesButton: false
}

var sunFunction= function(e) {
  e.stopPropagation();
  var sunThemes = Docsify.dom.find('[rel="stylesheet"][title="sun"]');
  var moonThemes = Docsify.dom.find('[rel="stylesheet"][title="moon"]');

  sunThemes.disabled = false;
  moonThemes.disabled = true;
};

var moonFunction= function(e) {
  e.stopPropagation();
  var sunThemes = Docsify.dom.find('[rel="stylesheet"][title="sun"]');
  var moonThemes = Docsify.dom.find('[rel="stylesheet"][title="moon"]');

  sunThemes.disabled = true;
  moonThemes.disabled = false;
};

// Docsify plugin functions
function plugin(hook, vm) {
  var userOptions = vm.config.jxthemesmanage;

  hook.mounted(function () {
    var mainElm = document.querySelector("main");
    var content = window.Docsify.dom.find(".content");
    if (content && userOptions.showThemesButton) {
      var jxSun = window.Docsify.dom.create("span", "<i class='fas fa-sun'></i>");
      jxSun.id = "jx-themes-manage-sun";
      jxSun.onclick = sunFunction;
      window.Docsify.dom.before(mainElm, jxSun);

      var jxMoon = window.Docsify.dom.create("span", "<i class='fas fa-moon'></i>");
      jxMoon.id = "jx-themes-manage-moon";
      jxMoon.onclick = moonFunction;
      window.Docsify.dom.before(mainElm, jxMoon);
    }
  });
}

// Docsify plugin options
window.$docsify['jx-themes-manage'] = Object.assign(defaultOptions, window.$docsify['jx-themes-manage']);
window.$docsify.plugins = [].concat(plugin, window.$docsify.plugins);
