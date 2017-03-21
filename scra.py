
import feedparser

d = feedparser.parse('http://www.clarin.com/rss/lo-ultimo/')
# .. skipped handling http errors, cacheing ..

for e in d.entries:
    print(e.title)
    print(e.link)
    #print(e.description)
    print("\n") # 2 newlines
