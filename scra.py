
import feedparser

d = feedparser.parse('http://www.laizquierdadiario.com/spip.php?page=backend_portada')
# .. skipped handling http errors, cacheing ..

for e in d.entries:
    print(e.title)
    print(e.link)
    #print(e.description)
    print("\n") # 2 newlines
