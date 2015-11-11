package dockeroo

import org.scalatra.test.specs2._

// For more on Specs2, see http://etorreborre.github.com/specs2/guide/org.specs2.guide.QuickStart.html
class EchoServletSpec extends ScalatraSpec { def is =
  "GET / on EchoServlet"                     ^
    "should return status 200"                  ! root200^
                                                end

  addServlet(classOf[EchoServlet], "/*")

  def root200 = get("/") {
    status must_== 200
  }
}
