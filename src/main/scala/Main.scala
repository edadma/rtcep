package ca.hyperreal.rtcep

import java.io._


object Main extends App
{
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
// 			ignore( new BlockCommentLexeme("/*", "*/", "unclosed comment") )
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
// 		}
// 		
// 	println( l.scan(new StringReader("1 a + **")).mkString(",") )
//	println( Lexer.chrStream(new StringReader("a\nb"), 4).mkString(",") )
	
	val p =
		new Parser[Any]( 4 )
		{
			def primary( value: Token ) = (value.kind, value.s)
			
			def structure( functor: Token, args: IndexedSeq[Any] ) = (functor.s, args.mkString("[", ",", "]"))
			
			add( 1000, 'xfy, "," )
			add(  700, 'xfx, "=", "\\=", "==", "=\\=" )
			add(  500, 'yfx, "+", "-" )
			add(  400, 'yfx, "*", "/" )
			add(  100, 'yf,  "!" )
			add(  200,  'fy, "+", "-" )
			add(  200, 'xfy, "^" )
		}
		
	println( p.parse(new StringReader("A = 123")) )
}