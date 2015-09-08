from lxml import html
import requests

page = requests.get('http://www.nytimes.com/interactive/2015/02/17/upshot/what-do-people-actually-order-at-chipotle.html?src=me&_r=1&abt=0002&abg=1')
tree = html.fromstring(page.text)

textbody = tree.xpath('//p[@title="paragraph paragraph-1"]/text()')
print textbody
