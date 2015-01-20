package ca.hyperreal.rtcep

import org.scalatest._
import prop.PropertyChecks


class ParserTest extends FreeSpec with PropertyChecks with Matchers
{
	val p =
		new Parser[String]( 4 )
		{
			def primary( value: Token ) = value.s
			
			def structure( functor: Token, args: IndexedSeq[String] ) =
				if (args.length == 1)
					s"${functor.s}${args(0)}"
				else
					s"${functor.s}(${args.mkString(",")})"
			
			add( 1000, 'xfy, "," )
			add(  700, 'xfx, "=", "\\=", "==", "=\\=" )
			add(  500, 'yfx, "+", "-" )
			add(  400, 'yfx, "*", "/" )
			add(  100, 'yf,  "!" )
			add(  200,  'fy, "+", "-" )
			add(  200, 'xfy, "^" )
		}
		
	val ERROR = "(.*) \\(.*\n.*\n.*"r
	
	def parse( s: String ) =
		try
		{
			p.parse( s )
		}
		catch
		{
			case e: Exception => e.getMessage match {case ERROR(msg) => msg}
		}
	
	"basic" in
	{
		parse( "1+2" ) shouldBe "+(1,2)"
		parse( "1+2*3" ) shouldBe "+(1,*(2,3))"
		parse( "(1+2)*3" ) shouldBe "*(+(1,2),3)"
		parse( "-1^2" ) shouldBe "-^(1,2)"
		parse( "1^-2" ) shouldBe "^(1,-2)"
		parse( "1!" ) shouldBe "!1"
		parse( "-1!" ) shouldBe "-!1"
		parse( "1 2" ) shouldBe "syntax error: expected operator"
	}
}