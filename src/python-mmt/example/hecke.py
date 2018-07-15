import mmt

# include db/hmfs?hecke
  
deg = 2
dim = 2
  
query = ??? # = lmfd.find(`db/hmfs?forms where (deg f)=deg, (dim f)=dim)

formset = mmt.controller.evaluator()(query) # : SetResult <: QueryResult
forms = [x.t()[0] for x in formset.s()]

# desirable syntax: something like:
forms = mmt.lmfdb.forms.find(deg=deg, dim=dim)

# This would returns a list of records/object
# accessing the hecke polynomial of the first form
forms[0].hecke_polynomial  # already converted to a Sage polynomial
# P=QQ["x"]
# P(<list of coefficients>)

for form in forms:
	# maybe we would want to copy form before changing it
	p = form.hecke_polynomial
	form.hecke_field = H = NumberField(p, p.parent().gen())
	form.disk = H.disc()

disk_counts = Word(form.disk for form in forms).evaluation_dict()

# rest as in the use case document

	
#  // do an lmfdb query
#  forms : list hilbertNewform ❘ // = lmfd.find(`db/hmfs?forms where deg=deg, dim=dim) ❙ 
#  labels : list string ❘ // = map forms hilbert_label
#  heckes : list hilbertNewform ❘ // = map labels lmfd.get(`db/hmfs?forms)
  
#  // this is basically an elaborate fornt of:
#  heckes : list hilbertNewform ❘ // = lmfd.find(`db/hmfs?hecke where deg=deg, dim=dim)
  
#  // ask sage for:  for p in heckes : NumberField("hecke_polynomial p", p.parent().gen()) <- add to record
#  // ask sage for:  for p in heckes : ("number_field p").disc() <- add to record