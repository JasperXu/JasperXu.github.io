;(function(win) {
  function isFunction(functionToCheck) {
   return functionToCheck && {}.toString.call(functionToCheck) === '[object Function]'
  }

  win.EditOnGithubPlugin = {}

  function create(docBase, docEditBase, title) {
    title = title || 'Edit on github'
    docEditBase = docEditBase || docBase.replace(/\/blob\//, '/edit/')

    function editDoc(event, vm) {
      var docName = vm.route.file

      if (docName) {
        var editLink = docEditBase + docName
        window.open(editLink)
        event.preventDefault()
        return false
      } else {
        return true
      }
    }

    win.EditOnGithubPlugin.editDoc = editDoc

    function generateHeader(title) {
      return header = [
        '<div style="overflow: auto">',
        '<p>',
        '修订时间：<i class="far fa-calendar-alt"></i> {docsify-updated} ',
        '<a style="float: right;text-decoration: underline; cursor: pointer"',
        'onclick="EditOnGithubPlugin.onClick(event)">',
        title,
        '</a></p>',
        '</div>\n\n'
      ].join('')
    }

    return function(hook, vm) {
      win.EditOnGithubPlugin.onClick = function(event) {
        EditOnGithubPlugin.editDoc(event, vm)
      }
      if (isFunction(title)) {
        hook.beforeEach(function (html) {
          return generateHeader(title(vm.route.file)) + html
        })
      } else {
        var header = generateHeader(title)
        hook.beforeEach(function (html) {
          return header + html
        })
      }


    }
  }

  win.EditOnGithubPlugin.create = create
}) (window)
