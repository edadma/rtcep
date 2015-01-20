package ca.hyperreal.rtcep

import java.io._


object Main extends App
{
//	println( Lexer.chrStream(new StringReader("a\nb"), 4).mkString(",") )
	
// 	val l =
// 		new Lexer( 4 )
// 		{
// 			add(
// 				new AtomLexeme
// 				{
// 					add( "and" )
// 				} )
// 			add( NumberLexeme )
// 			ignore( new LineCommentLexeme("%") )
// 			ignore( new BlockCommentLexeme("/*", "*/") )
// 			add( StringLexeme )
// 			add(
// 				new SymbolLexeme
// 				{
// 					add( "+" )
// 					add( "*" )
// 					add( "**" )
// 					add( "(" )
// 					add( ")" )
// 				} )
// 			ignore( WhitespaceLexeme )
// 			add( EOFLexeme )
// 		}
// 		
// 	println( l.scan(new StringReader("""1 "as\\df" b""")).mkString(",") )
	
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
		
	println( p.parse(new StringReader("""(1) 2""")) )
}