package ca.hyperreal.rtcep

import java.io.{Reader, StringReader}

import collection.mutable.{ArrayBuffer, ArrayStack, HashMap}


abstract class Parser[A]( tabs: Int )
{
	def primary( value: Token ): A
	
	def structure( functor: Token, args: IndexedSeq[A] ): A
	
	private val atom =
		new AtomLexeme
		{
			
		}
	private val symbol =
		new SymbolLexeme
		{
			add( "(", ")", ".", "," )
		}
	private val l =
		new Lexer( tabs )
		{
			add( atom )
			add( NumberLexeme )
			add( VariableLexeme )
			ignore( new LineCommentLexeme("%") )
			ignore( new BlockCommentLexeme("/*", "*/") )
			add( StringLexeme )
			add( symbol )
			ignore( WhitespaceLexeme )
			add( EOFLexeme )
		}
		
	private val operators = new ArrayBuffer[Operator]
	private val opmap = new HashMap[Any, Map[Symbol, Operator]]
	
	def add( prec: Int, assoc: Symbol, names: String* )
	{
		require( prec > 0, "precedence is positive" )
		require( Set('xf, 'yf, 'fx, 'fy, 'xfx, 'xfy, 'yfx) contains assoc, "invalid associativity: " + assoc )
	
		for (name <- names)
		{
		val tok =
			if (name.length == 1)
				name.head
			else
				Symbol(name)
				
		val fixity =
			if (Set('fx, 'fy)(assoc))
				'prefix
			else if (Set('xfx, 'xfy, 'yfx)(assoc))
				'infix
			else
				'postfix
		val op = Operator( tok, prec, assoc )

			operators.indexWhere( _.prec < prec ) match
			{
				case -1 => operators += op
				case ind => operators.insert( ind, op )
			}
			
			opmap(tok) =
				if (opmap contains tok)
					if ((opmap(tok) contains fixity) || fixity == 'infix && (opmap(tok) contains 'postfix) ||
						fixity == 'postfix && (opmap(tok) contains 'infix))
						sys.error( "operator with similar fixity already added: " + (prec, assoc, name) )
					else
						opmap(tok) + (fixity -> op)
				else
					Map(fixity -> op)
					
			if (name.head.isLetter)
			{
				if (!atom.reserved( name ))
					atom add name
			}
			else
			{
				if (!symbol.reserved( name ))
					symbol add name
			}
		}
	}
	
	def parse( s: String ): A = parse( new StringReader(s) )
	
	def parse( r: Reader ): A =
	{
	val argstack = new ArrayStack[A]
	
		def pop1( tok: Token ) =
			if (argstack.isEmpty)
				tok.pos.error( "syntax error: missing operand" )
			else
				IndexedSeq( argstack.pop )
		
		def pop2( tok: Token ) =
			if (argstack.size < 2)
				tok.pos.error( "syntax error: missing operand" + (if (argstack.isEmpty) "s" else "") )
			else
			{
			val top = argstack.pop
			
				IndexedSeq( argstack.pop, top )
			}
		
	val opstack = new ArrayStack[Operation]
	var toks = l.scan( r )
 	var prev: Any = null
		
		while (!toks.head.end)
		{
		val tok = toks.head
		
			toks = toks.tail

			tok.kind match
			{
				case '(' =>
					prev = null
					opstack push Operation( tok, 10000, null, 'lparen )
				case ')' =>
					while (!opstack.isEmpty && opstack.top.tok.kind != '(')
					{
						opstack.pop match
						{
							case Operation(optok, _, _, 'prefix) => argstack push structure( optok, pop1(optok) )
							case Operation(optok, _, _, 'infix) => argstack push structure( optok, pop2(optok) )
						}
					}
					
					if (opstack.isEmpty)
						tok.pos.error( "syntax error: unmatched closing parenthesis" )
						
					prev = tok
					opstack.pop
				case k =>
					opmap.get( k ) match
					{
						case None =>
							if (prev != null && prev.isInstanceOf[Token])
								tok.pos.error( "syntax error: expected operator" )
								
							prev = tok
							argstack push primary( tok )
						case Some( map ) =>
							val op =
								if (prev != null && (prev.isInstanceOf[Token] || prev.asInstanceOf[Operation].fixity == 'postfix))
									map.get('infix) match
									{
										case None =>
											map.get('postfix) match
											{
												case None => tok.pos.error( "syntax error: unexpected prefix operator" )
												case Some( o ) => Operation( tok, o.prec, o.assoc, 'postfix )
											}
										case Some( o ) => Operation( tok, o.prec, o.assoc, 'infix )
									}
								else
									map.get('prefix) match
									{
										case None => tok.pos.error( "syntax error: unexpected infix/postfix operator" )
										case Some( o ) => Operation( tok, o.prec, o.assoc, 'prefix )
									}
								
							while (!opstack.isEmpty &&
								(
								op.assoc == 'yfx && opstack.top.prec <= op.prec ||
								op.assoc == 'xfy && opstack.top.prec < op.prec ||
								op.assoc == 'xfx && opstack.top.prec < op.prec ||
								op.assoc == 'yf && opstack.top.prec <= op.prec ||
								op.assoc == 'xf && opstack.top.prec < op.prec
								))
							{
								opstack.pop match
								{
									case Operation(optok, _, _, 'prefix) => argstack push structure( optok, pop1(optok) )
									case Operation(optok, _, _, 'infix) => argstack push structure( optok, pop2(optok) )
								}
							}
							
							prev = op
							
							if (op.fixity == 'postfix)
								argstack push structure( op.tok, pop1(op.tok) )
							else if (opstack.isEmpty || opstack.top.tok.kind == '(' ||
								(
								opstack.top.assoc == 'yfx ||
								opstack.top.assoc == 'xfx ||
								opstack.top.assoc == 'fx) && op.prec < opstack.top.prec ||
								(
								opstack.top.assoc == 'xfy ||
								opstack.top.assoc == 'fy) && op.prec <= opstack.top.prec
								)
								opstack push op 
							else
								op.tok.pos.error( "syntax error: operator priority clash" )
					}
			}
		}
		
		while (!opstack.isEmpty)
		{
			opstack.pop match
			{
				case Operation(optok, _, _, 'prefix) => argstack push structure( optok, pop1(optok) )
				case Operation(optok, _, _, 'infix) => argstack push structure( optok, pop2(optok) )
				case Operation(Token('(', _, pos), _, _, _) => pos.error( "syntax error: unmatched open parenthesis" )
			}
		}
		
		argstack.pop
	}

	case class Operator( tok: Any, prec: Int, assoc: Symbol )

	case class Operation( tok: Token, prec: Int, assoc: Symbol, fixity: Symbol )
}