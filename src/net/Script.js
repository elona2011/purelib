exports._addScript = function(url) {
  return function(left) {
    return function(right) {
      return function(cb) {
        return function() {
          var head = document.getElementsByTagName('head')[0],
            script = document.createElement('script')

          script.type = 'text/javascript'
          script.charset = 'UTF-8'
          script.async = true
          script.onerror = function(err) {
            if (head.contains(script)) {
              head.removeChild(script)
            }
            cb(left(err))
          }

          var done = false
          script.onload = script.onreadystatechange = function(e) {
            if (
              !done &&
              (!this.readyState || this.readyState === 'loaded' || this.readyState === 'complete')
            ) {
              done = true
              setTimeout(function() {
                cb(right(e))
              }, 0)
            }
          }
          script.src = url
          head.appendChild(script)

          return {}
        }
      }
    }
  }
}
