package info.kwarc.mmt.MitM.Config

import info.kwarc.mmt.MitM.Server.MiTMExtension
import info.kwarc.mmt.MitM.VRESystem.VRESystem
import info.kwarc.mmt.api.GeneralError
import info.kwarc.mmt.api.frontend.actions.{Action, ActionCompanion, ActionState, ResponsiveAction}
import info.kwarc.mmt.api.utils.{File, MMTSystem}
import info.kwarc.mmt.odk._

trait Actions {
  this: Plugin =>

  var config: Option[MitMConfig] = None

  case class LoadMitMConfig(file: Option[String]) extends Action with ResponsiveAction {
    def apply(): Unit = {

      // if we have already loaded a config file, exit
      if(config.isDefined) {
        respond("MitM configuration already loaded")
        return
      }

      // try and read the configuration file
      val cfgFileContents = file match {
        case None => MMTSystem.getResourceAsString("mitm/config.default.json")
        case Some(fn) => File.read(File(fn))
      }

      try {
        // create the configuration object
        config = Some(MitMConfig(cfgFileContents))

        log("loaded mitm configuration: ")
        logGroup {
          log(s"GAP at:      ${config.get.gap}")
          log(s"Sage at:     ${config.get.sage}")
          log(s"Singular at: ${config.get.singular}")
        }

        // and start all the extensions
        log("starting system extensions")
        controller.extman.addExtension(new LMFDB.Plugin)
        controller.extman.addExtension(new GAP.Plugin)
        controller.extman.addExtension(new Sage.Plugin)
        controller.extman.addExtension(new Singular.Plugin)

        // and run the warmup code
        log("warming up caches, this might take some time. ")

        logGroup {
          controller.extman.get(classOf[VRESystem]).foreach({ v =>
            log(s"warming up ${v.id}")
            v.warmup()
          })
        }

        log("Done, configuration applied. ")

      // if things fail, remove the extensions a  again
      } catch {
        case e: Exception =>
          respond("Configuration loading failed: You should probably fix issues and restart MMT. ")
          config = None

          controller.extman.get(classOf[VRESystem]).foreach(controller.extman.removeExtension)

          throw GeneralError("Activating MitMConfig failed").setCausedBy(e)
      }

    }

    def toParseString: String = s"mitm use ${file.getOrElse("")}".trim
  }


  object MitMConfigActionCompanion extends ActionCompanion("loads MiTM Configuration", "mitm use") with MiTMExtension {

    import Action._

    def parserActual(implicit state: ActionState) = (str?) ^^ { s => LoadMitMConfig(s)}
  }



}
