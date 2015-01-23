package ca.hyperreal.rtcep

import java.io.{Reader, StringReader}

import collection.mutable.{ArrayBuffer, ArrayStack, HashMap}


object TestParser extends AbstractPrologParser[String]
{
	def primary( value: Token ) =
		value.kind match
		{
			case 'atom => "'" + value.s + "'"
			case 'charlist => '"' + value.s + '"'
			case 'string => '`' + value.s + '`'
			case _ => value.s
		}
	
	def structure( functor: Token, args: IndexedSeq[Value[String]] ) =
		if (args.length == 1 && functor.kind != 'atom)
			functor.s + args(0).v
		else
			s"${functor.s}(${args.map(_.v).mkString(",")})"
}

abstract class AbstractPrologParser[A] extends AbstractParser[A]( 4 )
{
	symbols.add( ".", "[]", "[", "]", "|" )
	add( 1000, 'xfy, "," )
	add(  700, 'xfx, "=", "\\=", "==", "=\\=" )
	add(  500, 'yfx, "+", "-" )
	add(  400, 'yfx, "*", "/" )
	add(  100, 'yf,  "!" )
	add(  200,  'fy, "+", "-" )
	add(  200, 'xfy, "^" )
}

abstract class AbstractParser[A]( tabs: Int ) extends Parser[A]
{
	protected val atoms = new AtomLexeme( 'atom )
	protected val symbols =
		new SymbolLexeme( 'atom )
		{
			add( "(", ")" )
		}
		
	override protected val lexer =
		new Lexer( tabs )
		{
			add( atoms )
			add( new StringLexeme('atom, '\'') )
			add( new StringLexeme('charlist, '"') )
			add( new StringLexeme('string, '`') )
			add( new FloatingLexeme('float) )
			add( new IntegerLexeme('integer) )
			add( new VariableLexeme('variable) )
			ignore( new LineCommentLexeme("%") )
			ignore( new BlockCommentLexeme("/*", "*/") )
			add( symbols )
			ignore( WhitespaceLexeme )
			add( EOFLexeme )
		}	
}

abstract class Parser[A]
{
	protected def primary( value: Token ): A
	
	protected def structure( functor: Token, args: IndexedSeq[Value[A]] ): A
	
	protected val atoms: KeywordLexeme
	protected val symbols: KeywordLexeme
	protected val lexer: Lexer
	
	protected val dotsym = Symbol( "." )
	protected val nilsym = Symbol( "[]" )
	
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
				if (!atoms.reserved( name ))
					atoms add name
			}
			else
			{
				if (!symbols.reserved( name ))
					symbols add name
			}
		}
	}
	
	def parse( s: String, endtok: Any ): (A, Stream[Token]) = parse( new StringReader(s), endtok )
	
	def parse( r: Reader, endtok: Any ): (A, Stream[Token]) =
	{
	val argstack = new ArrayStack[Value[A]]
	
		def pop( tok: Token ) =
			if (argstack.isEmpty)
				tok.pos.error( "syntax error: missing operand" )
			else
				argstack.pop
				
		def pop1( tok: Token ) = IndexedSeq( pop(tok) )
		
		def pop2( tok: Token ) =
			if (argstack.size < 2)
				tok.pos.error( "syntax error: missing operand" + (if (argstack.isEmpty) "s" else "") )
			else
			{
			val top = argstack.pop
			
				IndexedSeq( argstack.pop, top )
			}
		
	val opstack = new ArrayStack[Operation]
	var toks = lexer.scan( r )
 	var prev: Any = null
	val comma: Map[Symbol, Operator] = if (opmap contains ',') null else Map( 'infix -> Operator(',', 10000, 'xfy) )
	val bar: Map[Symbol, Operator] = if (opmap contains '|') null else Map( 'infix -> Operator('|', 10000, 'xfx) )
	
		while (!toks.head.end && toks.head.kind != endtok)
		{
		val tok = toks.head
		
			toks = toks.tail

			tok.kind match
			{
				case '[' =>
					if (prev.isInstanceOf[Token])
						prev.asInstanceOf[Token].pos.error( "syntax error: expected operator" )
					else
						opstack push Operation( tok, 10000, null, 'lst )
						
					prev = null
				case '(' =>
					if (prev.isInstanceOf[Token])
						if (prev.asInstanceOf[Token].kind == 'atom)
							opstack push Operation( tok, 10000, null, 'lst )
						else
							prev.asInstanceOf[Token].pos.error( "syntax error: expected atom or operator" )
					else
						opstack push Operation( tok, 10000, null, 'lparen )
						
					prev = null
				case ']' =>
					if (prev == null || prev.isInstanceOf[Operation] &&
						(prev.asInstanceOf[Operation].fixity == 'infix || prev.asInstanceOf[Operation].fixity == 'prefix))
						tok.pos.error( "syntax error: unexpected closing bracket" )
						
					while (!opstack.isEmpty && opstack.top.tok.kind != '[')
					{
						opstack.pop match
						{
							case Operation(optok, _, _, 'prefix) => argstack push Value( optok, structure( optok, pop1(optok) ) )
							case Operation(optok, _, _, 'infix) => argstack push Value( optok, structure( optok, pop2(optok) ) )
						}
					}

					if (opstack.isEmpty)
						tok.pos.error( "syntax error: unmatched closing bracket" )
					
					opstack.pop match
					{
						case o@Operation(optok, _, _, 'lst) =>
							if (o.restflag)
								o.rest = pop( optok )
							else
								o.buf += pop( optok )
							
							def list( l: List[Value[A]] ): Value[A] =
								l match
								{
									case hd :: tl =>
										Value( hd.tok, structure(Token(dotsym, ".", hd.tok.pos), IndexedSeq(hd, list(tl))) )
									case Nil =>
										if (o.restflag)
											o.rest
										else
											Value( tok, primary(Token(nilsym, "[]", tok.pos)) )
								}
							
							argstack push list( o.buf.toList )
						case _ =>
					}

					prev = tok
				case ')' =>
					if (prev == null || prev.isInstanceOf[Operation] &&
						(prev.asInstanceOf[Operation].fixity == 'infix || prev.asInstanceOf[Operation].fixity == 'prefix))
						tok.pos.error( "syntax error: unexpected closing parenthesis" )
						
					while (!opstack.isEmpty && opstack.top.tok.kind != '(')
					{
						opstack.pop match
						{
							case Operation(optok, _, _, 'prefix) => argstack push Value( optok, structure( optok, pop1(optok) ) )
							case Operation(optok, _, _, 'infix) => argstack push Value( optok, structure( optok, pop2(optok) ) )
						}
					}

					if (opstack.isEmpty)
						tok.pos.error( "syntax error: unmatched closing parenthesis" )
					
					opstack.pop match
					{
						case o@Operation(optok, _, _, 'lst) =>
							o.buf += pop( optok )
							val Value(t, _) = pop( optok )
							
							argstack push Value( t, structure(t, o.buf.toIndexedSeq) )
						case _ =>
					}

					prev = tok
				case k =>
					(
						if (k == ',' && comma != null)
							Some( comma )
						else if (k == '|' && bar != null)
							Some( bar )
						else
							opmap.get( k )
					) match
					{
						case None =>
							if (prev != null && prev.isInstanceOf[Token])
								tok.pos.error( "syntax error: expected operator" )
								
							prev = tok
							argstack push Value( tok, primary(tok) )
						case Some( map ) =>
							val op =
								if (prev != null && (prev.isInstanceOf[Token] || prev.asInstanceOf[Operation].fixity == 'postfix))
									map.get('infix) match
									{
										case None =>
											map.get('postfix) match
											{
												case None => tok.pos.error( "syntax error: unexpected prefix symbol" )
												case Some( o ) => Operation( tok, o.prec, o.assoc, 'postfix )
											}
										case Some( o ) => Operation( tok, o.prec, o.assoc, 'infix )
									}
								else
									map.get('prefix) match
									{
										case None =>
											tok.pos.error( "syntax error: unexpected " + (if (map contains 'infix) "in" else "post") + "fix symbol" )
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
									case Operation(optok, _, _, 'prefix) => argstack push Value( optok, structure( optok, pop1(optok) ) )
									case Operation(optok, _, _, 'infix) => argstack push Value( optok, structure( optok, pop2(optok) ) )
								}
							}
							
							prev = op
							
							if (op.tok.kind == '|' && !opstack.isEmpty && opstack.top.fixity == 'lst)
							{
								opstack.top.restflag = true
								opstack.top.buf += argstack.pop
							}
							else if (op.tok.kind == ',' && !opstack.isEmpty && opstack.top.fixity == 'lst)
								opstack.top.buf += argstack.pop
							else if (op.fixity == 'postfix)
								argstack push Value( op.tok, structure( op.tok, pop1(op.tok) ) )
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
								op.tok.pos.error( "syntax error: symbol priority clash" )
					}
			}
		}
		
		while (!opstack.isEmpty)
		{
			opstack.pop match
			{
				case Operation(optok, _, _, 'prefix) => argstack push Value( optok, structure( optok, pop1(optok) ) )
				case Operation(optok, _, _, 'infix) => argstack push Value( optok, structure( optok, pop2(optok) ) )
				case Operation(Token('(', _, pos), _, _, _) => pos.error( "syntax error: unmatched open parenthesis" )
				case Operation(Token('[', _, pos), _, _, _) => pos.error( "syntax error: unmatched open bracket" )
			}
		}
		
		if (argstack.size > 1)
			toks.head.pos.error( "syntax error: more than one value on stack" )
			
		(argstack.pop.v, toks)
	}

	case class Operator( tok: Any, prec: Int, assoc: Symbol )

	case class Value[A]( tok: Token, v: A )
	
	case class Operation( tok: Token, prec: Int, assoc: Symbol, fixity: Symbol )
	{
		lazy val buf = new ArrayBuffer[Value[A]]
		var restflag = false
		var rest: Value[A] = null
	}
}