# Python side of Java-Python bridge
# see info.kwarc.mmt.python.Py4JGateway (an MMT extension that must be added to MMT for the bridge to work) for the counterpart

from py4j.java_gateway import JavaGateway, JavaObject, GatewayParameters, CallbackServerParameters
import py4j

# create the gateway that communicates with the JVM
gwp = GatewayParameters(auto_field=True,auto_convert=True)
cbp = CallbackServerParameters()
gateway = JavaGateway(gateway_parameters=gwp,callback_server_parameters=cbp)

# MMT sets the entry point to be the MMT controller
controller = gateway.entry_point

# jvm yields access to Java namespace
mmt = gateway.jvm.info.kwarc.mmt
api = mmt.api

# everything below here are optional improvements to smoothen the Scala-Python integration
# they also provide examples how to use the bridge


# General remarks for handling from Python Scala features that are not present in the JVM anymore
#  companion object: fields are static methods, call as usual
#  nullary functions: () is mandatory
#  sequence arguments: needs a Seq, use Seq(pythonlist) conversion
#  default arguments: all arguments must be provided
#  implicit conversions: apply explicitly
#  magic functions: some correspondence established below
#  symbolic method names as operators: use .M below, some infix operators can be mapped to Python magic functions (see below)

# make the string representations of JVM objects nicer
def cut(s,at):
    if len(s) > at-3 or '\n' in s:
        return s.split("\n")[0][:at] + " ..."
    else:
        return s
JavaObject.__repr__ = lambda self: "<" + self.getClass().getName() + " object (JVM) " + cut(self.toString(), 100) + ">"

# handle special identifiers: $ is not an idchar in Python
# use x.M(s) to access method s of x if x has special characters (Python does not allow $ in identifiers)

dollarReplacements = {"+":"$plus", "-": "$minus", "*": "$star", "/": "$div", "=": "$equals",
                      "!":"$bang", "?":"$qmark", "^":"$up"}
def dollarReplaceChar(c):
    if c in dollarReplacements:
        return dollarReplacements[c]
    else:
        return c
def dollarReplace(s):
    return "".join(map(dollarReplaceChar,s))
JavaObject.M = lambda self,s: py4j.java_gateway.get_method(self,dollarReplace(s))

# align magic functions (only work if they exist on the Scala side)
def MagicFun(s):
    return lambda self,*args,**kwargs: self.M(s)(*args,**kwargs)
JavaObject.__str__  = lambda self: self.toString()
JavaObject.__len__ = lambda self: self.length()
# () notation
JavaObject.__call__ = lambda self,*args,**kwargs: self.apply(*args,**kwargs)
# [] notation
JavaObject.__keys__ = lambda self: self.keys() # values for which getitem is defined, not standard in Scala
JavaObject.__getitem__ = lambda self,x: self.apply(x)
JavaObject.__setitem__ = lambda self,x,y: self.update(x,y)
JavaObject.__delitem__ = lambda self,x: self.delete(x)
# These lead to infinite recursions.
# . notation, not standard in Scala
# JavaObject.__dir__ = lambda self: self.attrs(x)
# JavaObject.__getattr__ = lambda self,x: self.getattr(x)
# JavaObject.__setattr__ = lambda self,x,y: self.setattr(x,y)
# JavaObject.__delattr__ = lambda self,x: self.delattr(x,y)
# in test
JavaObject.__contains__ = lambda self,x: self.contains(x)
# iteration
JavaObject.__iter__ = lambda self: self.iterator()
# needed because we can't inline it in Python
def iternext(o):
    if o.hasNext():
        return o.next()
    else:
        raise StopIteration
JavaObject.__next__ = iternext
JavaObject.__add__ = MagicFun("+")
JavaObject.__sub__ = MagicFun("-")
JavaObject.__mul__ = MagicFun("*")
JavaObject.__mod__ = MagicFun("%")
JavaObject.__truediv__ = MagicFun("/")
JavaObject.__lt__ = MagicFun("<")
JavaObject.__le__ = MagicFun("<=")
JavaObject.__eq__ = MagicFun("==")
JavaObject.__ne__ = MagicFun("!=")
JavaObject.__gt__ = MagicFun(">")
JavaObject.__ge__ = MagicFun(">=")

# TODO:
# it appears impossible to construct a BigInt and send it to a Scala function
# Py4J auto-converts it to a Python int, which is then converted to Java int,
# which then (even if big enough) does not match the function signature

# convert collections
jc = gateway.jvm.scala.collection.JavaConverters
def Seq(l):
    return jc.asScalaBufferConverter(l).asScala().toSeq()
def List(l):
    return jc.asScalaBufferConverter(l).asScala().toList()
def Map(m):
    return jc.mapAsScalaMapConverter(m).asScala()
def LMap(m):
    return jc.mapAsScalaMapConverter(m).asScala().toList()
