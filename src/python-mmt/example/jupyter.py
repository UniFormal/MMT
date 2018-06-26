from mmt import *

# scala = mmt.python.JupyterKernel()
# extman().addExtension(scala)

scala = controller.extman().addExtension("info.kwarc.mmt.python.JupyterKernel", List([]))

class Widget:
    def __init__(self,n):
        self.n = n
        self.keys = {}
    def name(self):
        return self.n
    def set(self, k,v):
        self.keys[k]=v
    def get(self, k):
        return self.keys[k]
    class Java:
        implements = ["info.kwarc.mmt.python.WidgetPython"]

class JupyterKernel:
    def __init__(self,widgets):
        self.widgets = widgets
    def getWidgets(self):
        return self.widgets
    def addWidget(self,w):
        self.widgets.append(w)
    def removeWidget(self,n):
        self.widgets.pop()
    class Java:
        implements = ["info.kwarc.mmt.python.JupyterKernelPython"]



def test():
    w = Widget("hello")
    w.set("hello","world")

    kernel = JupyterKernel(List([w]))

    req = "show"
    print("before: ", w.get("hello"))
    response = scala.processRequest(kernel,req)
    print("response: ", response)
    print("after: ", w.get("hello"))
