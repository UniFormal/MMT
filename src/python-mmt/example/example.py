from mmt import *

controller.handleLine("server on 8080", True)

lf = api.Path.parse("http://cds.omdoc.org/urtheories?LF")
LF = controller.get(lf)

c = api.objects.Context.empty()
cc = c.M("++")(c)

utils = api.utils 
j = utils.JSONInt(5)
a = utils.JSONArray(Seq([j,j]))
J = utils.JSONString
o = utils.JSONObject(LMap({J("x"):j, J("y"):j}))
e = o("x")

