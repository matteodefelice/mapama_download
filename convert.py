import pandas as pd
import sys
from xml.sax import ContentHandler, parse

# Reference https://goo.gl/KaOBG3
class ExcelHandler(ContentHandler):
    def __init__(self):
        self.chars = [  ]
        self.cells = [  ]
        self.rows = [  ]
        self.tables = [  ]
    def characters(self, content):
        self.chars.append(content)
    def startElement(self, name, atts):
        if name=="Cell":
            self.chars = [  ]
        elif name=="Row":
            self.cells=[  ]
        elif name=="Table":
            self.rows = [  ]
    def endElement(self, name):
        if name=="Cell":
            self.cells.append(''.join(self.chars))
        elif name=="Row":
            self.rows.append(self.cells)
        elif name=="Table":
            self.tables.append(self.rows)

excelHandler = ExcelHandler()

parse(sys.argv[1], excelHandler)
df1 = pd.DataFrame(excelHandler.tables[0][2:], columns=excelHandler.tables[0][1])
df1.to_csv(sys.argv[2])


