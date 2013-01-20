This contains several MMT-related projects.

***************** Overview *****************

mmt-api: the main project

mmt-X: plugins for MMT, specifically:
  * lf: LF type checking and importer for Twelf
  * lfs: LFS type checking
  * reflection: type checking for type theory with reflection
  * mizar: importer for Mizar
  * owl: importer for OWL
  * tptp: importer for TPTP
  * mathscheme: importer for MathScheme

X-mmt: MMT acting as plugin or service to other applications, specifically
  * jEdit: MMT editing support in jEdit
  * hets: MMT logic compilation for Hets

lfcatalog: a non-MMT-related project for the Twelf catalog, which is used by Twelf (and thus by mmt-lf)
tiscaf: a copy of the HTTP server tiscaf, which MMT uses, with a couple of fixes and improvements

All projects are built into one jar file "projectname.jar" each (Exception: jEdit is built into MMTPlugin.jar according to jEdit conventions.).
These are deployed in the deploy folder, from where they can be run directly.

If projects have branches (usually only mmt-api), their folder contains "trunk" and "branches" folders, each containing separate eclipse projects.
Generally, the trunk is stable, and the branches are experimental.

***************** Building with Eclipse *****************

All projects are committed together with eclipse project files.

These permit importing the needed projects into Eclipse via "Import - Existing Projects into Workspace".
(Do not copy the source into your workspace.)

Most projects depend on the project "deploy-wrapper" in the deploy directory.
In that case, you have to import "deploy-wrapper" as well.
This dummy project is used to conveniently import the jars of the other projects.

All projects receive their dependencies
  * for project-specific external dependencies: the project's lib directory
  * for other MMT projects: through the deploy-wrapper project
    Thus, each project can be built individually. (Exception: Occassionally, projects have a direct dependency on mmt-api.)

***************** Building with ant *****************
    
All projects are committed together with ant build.xml files.

These permit building each project using simply "ant" (long form: "ant build.xml" deploy).
Specifically, the targets are compile, build (produces the jar), and deploy (builds and copies the jar to the deploy folder).

All build scripts receive their dependencies (including the used Scala compiler) from the deploy directory.
Thus, when building a project, all dependencies must be built (and deployed) first.
