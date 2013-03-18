url.root = "/gne/home/sandmat1/rapache-test/htdocs"

gcmap <-Builder$new(
  Static$new(
    urls = c('/css','/img','/js'),
    root = url.root
  ),
  Static$new(urls='/results',
             root=tempdir()
  ),
  Brewery$new(
    url='/',
    root=url.root,
    resultpath=tempdir()
  ),
  Redirect$new('/index.rhtml')
)
