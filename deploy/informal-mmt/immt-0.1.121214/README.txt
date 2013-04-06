Release of the MMT-API system, for more information see https://trac.kwarc.info/MMT

Contents: 
mmt-api.jar    -- the mmt-api system
jar-lib/       -- folder containing mmt-api's dependencies
  --/lfcatalog -- for loading existing lf libraries on the filesystem
  --/external  -- external dependencies (e.g. scala-compiler)
  --/mmt       -- mmt-related dependencies (e.g. mmt-lf for features specific to the LF foundation)   
doc-lib/       -- document library containing bootstrap and example mmt documents
  --/styles    -- library of MMT styles use for document presentation (e.g. html rendering)
  --/theories  -- mmt archive containing the LF foundation and basic theories
  --/sample    -- sample archive containing simple LF-based theory declarations    


To run use (on Linux) : 
./run.sh <config-file>
The default config file is: default-config.msl
It loads the attached doc libraries and starts the MMT server on localhost:8080

Once server is started it is ready to recieve and answer queries. For the query language 
specification check the documentation at https://trac.kwarc.info/MMT
example queries: 
-- Get an existing theory (Peano) by URI: 
   http://localhost:8080/:mmt?http://latin.omdoc.org/math?Peano??

-- Render existing theory (Peano) with mathml-based style twelf:  
   http://localhost:8080/:mmt?http://latin.omdoc.org/math?Peano??_present_http://cds.omdoc.org/styles/lf/mathml.omdoc?twelf

-- Add a theory (NatNrs) to the library via a POST request : 
   http://localhost:8080/:change 
   with Content-Type : text/html as header and the content of the sample-diff.omdoc file as body

Logging is disabled, to enable add: 
log console 
(for logging in the console) or 
log file <filename> 
(for logging in a file) in the config file. 
follow by
log+ <service>
where service can be e.g. (presenter, backend, controller, extman, reader, archive,
      checker, object-checker, query, catalog, server,  abox, parser,lf, etc..)
See api documentation for more details.

