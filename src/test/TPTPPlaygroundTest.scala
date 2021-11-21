import info.kwarc.mmt.api.ImplementationError
import info.kwarc.mmt.api.frontend.{Controller, Extension}
import leo.modules.input.TPTPParser

object TPTPPlaygroundTest extends MagicTest {

  override def doFirst(): Unit = {
    super.doFirst()
  }

  def run(): Unit = {
    controller.handleLine("log+ debug")
    val p = TPTPParser.problem("""thf(beverage_type,type,beverage: $tType).
                         |thf(syrup_type,type,syrup: $tType).
                         |thf(coffee_type,type,coffee: beverage).
                         |thf(mix_type,type,mix: beverage > syrup > beverage).
                         |thf(heat_type,type,heat: beverage > beverage ).
                         |thf(heated_mix_type,type,heated_mix: beverage > syrup > beverage).
                         |thf(hot_type,type,hot: beverage > $o).
                         |
                         |thf(heated_mix,axiom,
                         |    heated_mix = ( ^ [B: beverage,S :syrup] : ( heat @ ( mix @ B @ S ))) ).
                         |
                         |thf(hot_mixture,axiom,
                         |    ! [B: beverage,S: syrup] : ( hot @ ( heated_mix @ B @ S ) ) ).
                         |
                         |thf(heated_coffee_mix,axiom,
                         |    ! [S: syrup] : ( ( heated_mix @ coffee @ S ) = coffee ) ).
                         |
                         |thf(hot_coffee,conjecture,
                         |    ? [Mixture: syrup > beverage] :
                         |    ! [S: syrup] :
                         |        ( ( ( Mixture @ S ) = coffee )
                         |        & ( hot @ ( Mixture @ S ) ) ) ).""".stripMargin)
    println(p)
    if (!loadMMTExtensionFromArchives("latin2.tptp.TPTPExporter")(controller)) {
      throw ImplementationError("Extension cannot be loaded from archive!")
    }
    //controller.handleLine("build MMT/LATIN2 mmt-omdoc playground/tptp-exporter_monoid.mmt")
    //controller.handleLine("build MMT/LATIN2 tptp playground/tptp-exporter_monoid.omdoc")

    controller.handleLine("log+ debug")
    controller.handleLine("build MMT/LATIN2 mmt-omdoc playground/tptp-exporter_hol.mmt")
    controller.handleLine("build MMT/LATIN2 tptp playground/tptp-exporter_hol.omdoc")
  }

  /**
    * Instantiates an [[Extension]] (to be found in some loaded archive) and adds it to the [[Controller]].
    *
    * @param clazz Fully qualified Scala class name referencing the MMT extension (a Scala class)
    */
  private def loadMMTExtensionFromArchives(clazz: String)(implicit ctrl: Controller): Boolean = {
    ctrl.backend
      .loadClass(clazz)
      .map(_.getDeclaredConstructor().newInstance().asInstanceOf[Extension])
      .map(ctrl.extman.addExtension(_))
      .isDefined
  }
}
