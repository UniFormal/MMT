package info.kwarc.mmt.mizar.mmtwrappers

import info.kwarc.mmt.mizar.translator._
import MizSeq._
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.lf._

object RegPatterns  {
  val MizExistentialReg = MizPattern(LocalName("MizExistentialRegistration"),
        Context(VarDecl(LocalName("n")),
              VarDecl(LocalName("argTypes"), Ellipsis(OMV("n"), LocalName("i"), Mizar.tp)),
              VarDecl(LocalName("typ"), Mizar.tp),
              VarDecl(LocalName("cluster"), Mizar.attr(OMV("typ")))),
        Context(VarDecl(LocalName("reg"), MMTUtils.PiArgs("x", "n", MMTUtils.PiArgTypes("x", "argTypes", "n",
            Mizar.proof(Mizar.exists("q",
                Mizar.adjective(OMV("cluster"), OMV("typ")),
                Mizar.constant("true")))))
            )))


  val MizFunctionalReg = MizPattern(LocalName("MizFunctionalRegistration"),
        Context(VarDecl(LocalName("n")),
              VarDecl(LocalName("argTypes"), Ellipsis(OMV("n"), LocalName("i"), Mizar.tp)),
              VarDecl(LocalName("functor"), Mizar.tp),
              VarDecl(LocalName("cluster"), MMTUtils.PiArgs("x", "n", MMTUtils.PiArgTypes("x", "argTypes", "n",
                  Mizar.attr(Mizar.apply(OMV("functor"), OMV("x"))))))),
        Context(VarDecl(LocalName("reg"), MMTUtils.PiArgs("x", "n", MMTUtils.PiArgTypes("x", "argTypes", "n",
            Mizar.proof(Mizar.exists("q",
                Mizar.adjective(OMV("cluster"), Mizar.apply(OMV("functor"), OMV("x"))),
                Mizar.constant("true")))))
            )))


  val MizConditionalReg = MizPattern(LocalName("MizConditionalRegistration"),
        Context(VarDecl(LocalName("n")),
              VarDecl(LocalName("argTypes"), Ellipsis(OMV("n"), LocalName("i"), Mizar.tp)),
              VarDecl(LocalName("typ"), Mizar.tp),
              VarDecl(LocalName("first"), Mizar.attr(OMV("typ"))),
              VarDecl(LocalName("second"), Mizar.attr(OMV("typ")))),
        Context(VarDecl(LocalName("reg"), MMTUtils.PiArgs("x", "n", MMTUtils.PiArgTypes("x", "argTypes", "n",
            Mizar.proof(Mizar.implies(
                Mizar.exists("q", Mizar.adjective(OMV("first"), OMV("typ")),Mizar.constant("true")),
                Mizar.exists("q", Mizar.adjective(OMV("second"), OMV("typ")),Mizar.constant("true"))))))
            )))
}

object SchemePatterns {
  val MizSchemeDef = MizPattern(LocalName("MizSchemeDef"),
                  Context(VarDecl(LocalName("n")),
                                VarDecl(LocalName("args"), Ellipsis(OMV("n"), LocalName("i"), Mizar.tp))) ++
                                Context(VarDecl(LocalName("m")),
                                VarDecl(LocalName("premises"), Ellipsis(OMV("m"), LocalName("i"), Mizar.prop))) ++
                               Context(VarDecl(LocalName("prop"), Mizar.prop)),
                  Context(VarDecl(LocalName("scheme"), MMTUtils.PiArgs("x", "n", MMTUtils.PiArgTypes("x", "args", "n",
                                    Mizar.proof(Mizar.implies(Mizar.seqConn("and", OMV("m"), OMV("premises")),OMV("prop")))))
                                      )))
}
