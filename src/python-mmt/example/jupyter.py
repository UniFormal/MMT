from mmt import *

# scala = mmt.python.JupyterKernel()
# extman().addExtension(scala)

scala = controller.extman().addExtension("info.kwarc.mmt.python.JupyterKernel", List([]))

class JupWidget:
    def __init__(self):
        self.cbs = {}
    def observe(self,f,k):
        self.cbs[k] = f
    def event(self, k, d):
        self.cbs[k](d)

class Widget:
    def __init__(self,w):
        self.jupwid = w
        self.keys = {}
    def set(self, k,v):
        self.keys[k]=v
    def get(self, k):
        return self.keys[k]
    def observe(self, key, callback):
        self.jupwid.observe(lambda d: callback(kernel, d), key)
    def toString(self):
        return repr(self)
    class Java:
        implements = ["info.kwarc.mmt.python.WidgetPython"]

j = JupWidget()

class JupyterKernel:
    def __init__(self,widgets):
        self.widgets = widgets
    def getWidgets(self):
        return List(self.widgets)
    def addWidget(self,w):
        self.widgets.append(w)
    def removeWidget(self,n):
        
        self.widgets.pop()
    def makeWidget(self, kind):
        return Widget(j)
    class Java:
        implements = ["info.kwarc.mmt.python.JupyterKernelPython"]

kernel = JupyterKernel([])

req = "show"
#print("before: ", w.get("hello"))
response = scala.processRequest(kernel,req)
#print("response: ", response)
#print("after: ", w.get("hello"))
