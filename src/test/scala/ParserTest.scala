package ca.hyperreal.rtcep

import org.scalatest._
import prop.PropertyChecks


class ParserTest extends FreeSpec with PropertyChecks with Matchers
{
	val p =
		new Parser[Any]( 4 )
		{
			def primary( value: Token ) = value.s
			
			def structure( functor: Token, args: IndexedSeq[Any] ) =
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
		
	"basic" in
	{
		p.parse( "1+2" ) shouldBe "+(1,2)"
		p.parse( "1+2*3" ) shouldBe "+(1,*(2,3))"
		p.parse( "(1+2)*3" ) shouldBe "*(+(1,2),3)"
		p.parse( "-1^2" ) shouldBe "-^(1,2)"
		p.parse( "1^-2" ) shouldBe "^(1,-2)"
		p.parse( "1!" ) shouldBe "!1"
		p.parse( "-1!" ) shouldBe "-!1"
	}
}