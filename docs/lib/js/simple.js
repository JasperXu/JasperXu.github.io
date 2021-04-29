(function () {
  if (typeof document === "undefined") {
    return;
  }
  document.addEventListener("DOMContentLoaded", fixDOM);

  function fixDOM() {
    let CoverFlag = false;
    let ArticleFlag = false;
    // 清理Cover节点，标识是否含有该节点
    CoverFlag = clearCover();
    // 清理文章节点，标识是否含有文章节点
    ArticleFlag = clearArticle();

    if (ArticleFlag) {
      // 为文章添加浮动目录
      createTOC(3);
    }

    // 修复顶部的导航栏链接
    fixHeader2Bootstrap();

    if (ArticleFlag) {
      // 为文章底布添加上一章和下一章的按钮
      addPrevNext();

      // 修复侧边栏的链接
      fixAside2Bootstrap();

      //为文章添加评论功能，延迟1秒执行，避免执行过多卡顿
      // addGitalk();
      setTimeout(addGitalk, 1000);

      // 在导航中将本页标注为Active
      addActiveNav();
      // 为了性能可以节省下面的代码，会导致滚动时不会自动追踪active项
      $(window).scroll(() => {
        scrollAddActiveNav();
      });

      // 为锚点增加滚动动画
      fixAnchor();

      // 增加图片的缩放功能
      mediumZoom("article.article img", {
        margin: 64,
        background: "#7952b3",
      });
    }

    // 添加右上角的github链接
    addGithubCorner();

    // 设置导航内的下拉菜单hover打开
    $('.navbar [data-toggle="dropdown"]').bootstrapDropdownHover();

    // 设置Tab同步切换
    syncTab();
  }

  /**
   * Cover部分为空时，直接删除Cover部分。
   */
  function clearCover() {
    let temp = document.querySelector("section.cover > div.covermd");
    if (temp.innerHTML.trim() === "") {
      document.querySelector("section.cover").remove();
      return false;
    }

    return true;
  }

  /**
   * Article部分为空时，删除侧边栏和Article所在的content部分
   */
  function clearArticle() {
    let temp = document.querySelector("article.article");
    if (temp.innerHTML.trim() === "") {
      document.querySelector("aside.aside").remove();
      document.querySelector("section.content").remove();
      return false;
    }
    return true;
  }

  /**
   * 为顶部菜单调整结构，用于适应Bootstrap
   */
  function fixHeader2Bootstrap() {
    let header = $("body > header");
    header.addClass("navbar navbar-expand-md navbar-dark");
    let nav = $("nav.top-nav", header);
    nav.addClass("collapse navbar-collapse");
    let uls = $(">ul", nav);
    uls.addClass("navbar-nav ml-auto header-ul-root");
    let lis = $("li", nav);
    lis.addClass("nav-item");
    let alinks = $("a", nav);
    alinks.addClass("nav-link");
    let dropdownLis = $("li:has(ul)", nav);
    dropdownLis.addClass("dropdown");
    dropdownLis.each((index, element) => {
      let hasTextNodes = element.childNodes[0].nodeType === 3;
      if (hasTextNodes) {
        let wrapElm = document.createElement("a");
        wrapElm.setAttribute("href", "#");
        wrapElm.className = "nav-link";
        element.insertBefore(wrapElm, element.querySelector("ul"));
        while (element.childNodes[0] !== wrapElm) {
          wrapElm.appendChild(element.childNodes[0]);
        }
      }

      let a = $(">a", element);
      a.addClass("dropdown-toggle");
      a.attr({ "data-toggle": "dropdown", "aria-haspopup": "true", "aria-expanded": "false" });
      let ul = $(">ul", element);
      ul.removeClass("navbar-nav");
      ul.addClass("dropdown-menu").addClass("dropdown-menu-right");
      let lis = $(">li", ul);
      lis.removeClass("nav-item");
      let alinks = $(">a", lis);
      alinks.removeClass("nav-link");
      alinks.addClass("dropdown-item");
    });

    nav.removeClass("hide-nav");
    nav.addClass("show-nav");
  }

  /**
   * 为文章添加上一页和下一页，数据来源于侧边栏找到本页的link后找上一个和下一个
   */
  function addPrevNext() {
    let alinks = $("aside.aside a");
    let nowHref = decodeURI(window.location.pathname);
    let prevLink, nowLink, nextLink;
    for (let i = 0; i < alinks.length; i++) {
      const alink = $(alinks[i]);
      let fullHref = alink.attr("href");
      if (fullHref.indexOf("?") > 0) {
        fullHref = fullHref.substring(0, fullHref.indexOf("?"));
      }
      if (fullHref.indexOf("#") > 0) {
        fullHref = fullHref.substring(0, fullHref.indexOf("#"));
      }
      if (nowLink == undefined && fullHref != nowHref) {
        prevLink = alink;
      }
      if (fullHref == nowHref) {
        nowLink = alink;
      }
      if (nowLink != undefined && fullHref != nowHref && nextLink == undefined) {
        nextLink = alink;
        break;
      }
    }
    let PrevNext = '<div class="prev">';
    if (prevLink != undefined) {
      PrevNext += '<a href="' + prevLink.attr("href") + '">' + "<div class='pn-nav'>&lt; 上一章节</div>" + "<div><h3>" + prevLink.text() + "</h3></div></a>";
    }
    PrevNext += '</div><div class="next">';
    if (nextLink != undefined) {
      PrevNext += '<a href="' + nextLink.attr("href") + '">' + "<div class='pn-nav'>下一章节 &gt;</div>" + "<div><h3>" + nextLink.text() + "</h3></div></a>";
    }
    PrevNext += "</div>";
    $("section.content .prev-next").append(PrevNext);
  }

  /**
   * 为侧边栏菜单调整结构，用于适应Bootstrap
   */
  function fixAside2Bootstrap() {
    let aside = $("body > main aside.aside");
    let nav = $("nav.side-nav", aside);
    let uls = $(">ul", nav);
    uls.addClass("navbar-nav flex-column");
    let lis = $("li", nav);
    lis.addClass("nav-item");
    let alinks = $("a", nav);
    alinks.addClass("nav-link");
    let dropdownLis = $("li:has(ul)", nav);
    dropdownLis.each((index, element) => {
      let hasTextNodes = element.childNodes[0].nodeType === 3;
      if (hasTextNodes) {
        let wrapElm = document.createElement("a");
        wrapElm.setAttribute("href", "#");
        wrapElm.className = "nav-link";
        element.insertBefore(wrapElm, element.querySelector("ul"));
        while (element.childNodes[0] !== wrapElm) {
          wrapElm.appendChild(element.childNodes[0]);
        }
      }
    });

    nav.removeClass("hide-nav");
    nav.addClass("show-nav");
  }

  /**
   * 修复点击锚点后顶部被导航条遮挡的问题，向下移动80px
   */
  function fixAnchor() {
    // 修复tabs的a标签加上后会导致不工作
    $("header a[href*='#'], aside.aside a[href*='#'], #jx-toc a[href*='#'], article.article :header a[href*='#']").click(function () {
      if ($(this.hash) != null) {
        $("html, body").animate({ scrollTop: $(this.hash).offset().top }, 300);
      }
      // 启用滚动事件后可以不使用下面这行代码。
      //addActiveNav(IDName);
      return false;
    });
  }

  /**
   * 在导航中寻找离当前页面锚点最近的菜单对应的li，为其添加active样式
   */
  function addActiveNav(toHash) {
    let pathname = decodeURI(window.location.pathname);
    let hash = toHash ? toHash : decodeURI(window.location.hash);
    let top = $("nav.top-nav a[href='" + pathname + hash + "']");
    let side = $("nav.side-nav a[href='" + pathname + hash + "']");

    if (top.length == 0 && $("nav.top-nav .active").length == 0) {
      top = $("nav.top-nav a[href='" + pathname + "']");
    }
    if (side.length == 0 && $("nav.side-nav .active").length == 0) {
      side = $("nav.side-nav a[href='" + pathname + "']");
    }

    if (top.length > 0) {
      $("nav.top-nav .active").removeClass("active");
      // 顶部导航中包含本页的导航
      top.each(function (index, domEle) {
        let act = $(domEle);
        while (!act.hasClass("top-nav")) {
          act = act.parent();
          if (act.is("li")) {
            act.addClass("active");
          }
        }
      });
    }
    if (side.length > 0) {
      $("nav.side-nav .active").removeClass("active");
      // 侧边栏导航中包含本页的导航
      side.each(function (index, domEle) {
        let act = $(domEle);
        act = act.parent();
        act.addClass("active");
        if (!isElementInViewport(domEle)) {
          $("aside.aside").scrollTop(act[0].offsetTop);
        }
      });
    }
  }

  function isElementInViewport(el) {
    //获取元素是否在可视区域
    var rect = el.getBoundingClientRect();
    return (
      rect.top >= 0 &&
      rect.left >= 0 &&
      rect.bottom <= (window.innerHeight || document.documentElement.clientHeight) &&
      rect.right <= (window.innerWidth || document.documentElement.clientWidth)
    );
  }

  function scrollAddActiveNav() {
    let scrollTop = document.documentElement.scrollTop || document.body.scrollTop;
    let minEle, maxEle;
    let minSet = 0;
    let maxSet = 0;
    let activeEle;

    // 查找当前滚动条最近的锚点
    $("a.hiddenanchor").each(function (index, domEle) {
      let alink = $(domEle);
      // let tempTop = alink[0].offsetTop; //alink.offset().top;
      let tempTop = alink.offset().top; //alink.offset().top;
      if (tempTop > minSet && tempTop <= scrollTop) {
        minSet = tempTop;
        minEle = alink;
      }
      if (maxSet == 0 && tempTop >= scrollTop) {
        maxSet = tempTop;
        maxEle = alink;
      }
    });
    if (maxSet > 0 && maxSet - scrollTop < minSet - scrollTop) {
      activeEle = maxEle;
    } else {
      activeEle = minEle;
    }

    if (activeEle != undefined) {
      addActiveNav("#" + activeEle.attr("id"));
    }
  }

  /**
   * 创建一个浮动的目录
   */
  function createTOC(maxLevel) {
    let hs = $("article.article :header");
    let toc = document.createElement("div");
    toc.setAttribute("id", "jx-toc");
    $(".content").append(toc);

    var TocAnchor = document.createElement("i");
    TocAnchor.setAttribute("class", "fas fa-list");
    toc.appendChild(TocAnchor);

    let tocdiv = document.createElement("div");
    toc.appendChild(tocdiv);

    toc = $("#jx-toc div");
    for (let i = 0; i < hs.length; i++) {
      const h = $(hs[i]);
      let level = 0;
      if (h.is("h1")) {
        level = 1;
      } else if (h.is("h2")) {
        level = 2;
      } else if (h.is("h3")) {
        level = 3;
      } else if (h.is("h4")) {
        level = 4;
      } else if (h.is("h5")) {
        level = 5;
      } else if (h.is("h6")) {
        level = 6;
      }
      if (level <= maxLevel) {
        $("a", h)
          .clone()
          .css({ "margin-left": level * 16 + "px" })
          .appendTo(toc);
      }
    }
  }

  function addGithubCorner() {
    $("header").after(
      '<a href="https://github.com/jasperxu" target="_blank" class="github-corner fixed-top" aria-label="View source on GitHub">' +
        '  <svg width="64" height="64" viewBox="0 0 250 250" style="fill: #fff; color: #7952b3; position: absolute; top: 0; border: 0; right: 0" aria-hidden="true">' +
        '    <path d="M0,0 L115,115 L130,115 L142,142 L250,250 L250,0 Z"></path>' +
        "    <path" +
        '      d="M128.3,109.0 C113.8,99.7 119.0,89.6 119.0,89.6 C122.0,82.7 120.5,78.6 120.5,78.6 C119.2,72.0 123.4,76.3 123.4,76.3 C127.3,80.9 125.5,87.3 125.5,87.3 C122.9,97.6 130.6,101.9 134.4,103.2"' +
        '      fill="currentColor"' +
        '      style="transform-origin: 130px 106px"' +
        '      class="octo-arm"' +
        "    ></path>" +
        "    <path" +
        '      d="M115.0,115.0 C114.9,115.1 118.7,116.5 119.8,115.4 L133.7,101.6 C136.9,99.2 139.9,98.4 142.2,98.6 C133.8,88.0 127.5,74.4 143.8,58.0 C148.5,53.4 154.0,51.2 159.7,51.0 C160.3,49.4 163.2,43.6 171.4,40.1 C171.4,40.1 176.1,42.5 178.8,56.2 C183.1,58.6 187.2,61.8 190.9,65.4 C194.5,69.0 197.7,73.2 200.1,77.6 C213.8,80.2 216.3,84.9 216.3,84.9 C212.7,93.1 206.9,96.0 205.4,96.6 C205.1,102.4 203.0,107.8 198.3,112.5 C181.9,128.9 168.3,122.5 157.7,114.1 C157.9,116.9 156.7,120.9 152.7,124.9 L141.0,136.5 C139.8,137.7 141.6,141.9 141.8,141.8 Z"' +
        '      fill="currentColor"' +
        '      class="octo-body"' +
        "    ></path>" +
        "  </svg>" +
        "</a>"
    );
  }

  /**
   * 为文章添加评论功能
   */
  function addGitalk() {
    const gitalk = new Gitalk({
      clientID: "f4138bcafcce8b3e2a62",
      clientSecret: "bf3da43a9e862fca8252bf83491d103021eec268",
      repo: "jasperxu.github.io",
      owner: "jasperxu",
      admin: ["jasperxu"],
      id: getID(), // Ensure uniqueness and length less than 50
      distractionFreeMode: false, // Facebook-like distraction free mode
    });

    gitalk.render("gitalk-container");
  }

  /**
   *
   * 这里让所有的评论都汇集到同一个目录下。
   * @returns
   */
  function getID() {
    var idstring = decodeURI(window.location.pathname);
    // 这一段是合并一些同类的文件公用一个Issue
    // if (idstring.startsWith('/Note/Soft/Recommend')) return "/Note/Soft/Recommend";
    // if (idstring.startsWith('/Programming/Golang/GORM/')) return "/Programming/Golang/GORM/";
    // return idstring;
    return "/";
  }

  function syncTab() {
    let tabKey = [];
    let tabKeyAlinks = [];

    $('a[data-toggle="tab"]').each(function (index, domEle) {
      let tempalink = $(domEle);
      let keyIndex = tabKey.indexOf(tempalink.html());

      if (keyIndex == -1) {
        keyIndex = tabKey.length;
        tabKey.push(tempalink.html());
        tabKeyAlinks.push([]);
      }
      tabKeyAlinks[keyIndex].push(tempalink);
    });

    $('a[data-toggle="tab"]').on("shown.bs.tab", function (e) {
      let alink = $(e.target);
      let keyIndex = tabKey.indexOf(alink.html());
      tabKeyAlinks[keyIndex].forEach((item, index, array) => {
        item.tab("show");
      });
    });
  }
})();
