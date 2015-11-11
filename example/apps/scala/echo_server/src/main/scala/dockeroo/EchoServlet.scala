package dockeroo

import org.scalatra._
import scalate.ScalateSupport

class EchoServlet extends Echo_serverStack {

  get("/") {}

}
