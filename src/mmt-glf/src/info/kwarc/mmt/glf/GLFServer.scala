package info.kwarc.mmt.glf
import info.kwarc.mmt.api.web.{ServerExtension, ServerRequest, ServerResponse}

class GLFServer extends ServerExtension("glf"){
  def apply(request: ServerRequest): ServerResponse = {
    ServerResponse.TextResponse("Hello world")
  }
}

