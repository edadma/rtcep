package ca.hyperreal.rtcep

import org.scalatest._
import prop.PropertyChecks


class ParserTest extends FreeSpec with PropertyChecks with Matchers
{
	val p = TestParser
	
	val ERROR = "(.*) \\(.*\n.*\n.*"r
	
	def parse( s: String ) =
		try
		{
			p.parse( s, 4, EOF )._1
		}
		catch
		{
			case e: Exception => e.getMessage match {case ERROR(msg) => msg}
		}
	
	"basic" in
	{
		parse( "1 + 2" ) shouldBe "+(1,2)"
		parse( "1 + 2 * 3" ) shouldBe "+(1,*(2,3))"
		parse( "(1+2)*3" ) shouldBe "*(+(1,2),3)"
		parse( "1+(2+3)" ) shouldBe "+(1,+(2,3))"
		parse( "-1^2" ) shouldBe "-^(1,2)"
		parse( "1^-2" ) shouldBe "^(1,-2)"
// 		parse( "1!" ) shouldBe "!1"
// 		parse( "-1!" ) shouldBe "-!1"
	}
	
	"errors" in
	{
		parse( "1 2" ) shouldBe "syntax error: expected operator"
	}
	
	"lists" in
	{
		parse( "[1, 2, 3]" ) shouldBe ".(1,.(2,.(3,[])))"
		parse( "[1]" ) shouldBe ".(1,[])"
		parse( "[1, 2, 3|A]" ) shouldBe ".(1,.(2,.(3,A)))"
		parse( "[1|A]" ) shouldBe ".(1,A)"
		parse( "[1|[2]]" ) shouldBe ".(1,.(2,[]))"
		parse( "[1|A, B]" ) should startWith ("syntax error: comma not expected")
	}
	
	"comments" in
	{
		parse( "123%asdf\n+2" ) shouldBe "+(123,2)"
		parse( "123%asdf" ) shouldBe "123"
	}
}