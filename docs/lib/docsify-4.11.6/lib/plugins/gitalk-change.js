(function () {
  /* eslint-disable no-unused-vars */
  function install(hook) {
    var dom = Docsify.dom;

    hook.mounted(function (_) {
      var div = dom.create('div');
      div.id = 'gitalk-container';
      var main = dom.getNode('#main');
      div.style = "width: " + (main.clientWidth) + "px; margin: 0 auto 20px;";
      dom.appendTo(dom.find('.content'), div);
    });

    hook.doneEach(function (_) {
      var el = document.getElementById('gitalk-container');
      if (el != null) { // 去掉CoverPage的加载
        while (el.hasChildNodes()) {
          el.removeChild(el.firstChild);
        }

        const gitalk = new Gitalk({
          clientID: 'f4138bcafcce8b3e2a62',
          clientSecret: 'bf3da43a9e862fca8252bf83491d103021eec268',
          repo: 'jasperxu.github.io',
          owner: 'jasperxu',
          admin: ['jasperxu'],
          id: location.href.substring(location.origin.length+2),
          // facebook-like distraction free mode
          distractionFreeMode: false
        });
      
        // eslint-disable-next-line
        gitalk.render('gitalk-container');
      }
    });
  }

  $docsify.plugins = [].concat(install, $docsify.plugins);

}());
