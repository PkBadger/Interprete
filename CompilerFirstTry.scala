import scala.io.Source
import scala.util.control._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map

/*val INTEGER = "INTEGER"
val PLUS = "PLUS"
val MINUS = "MINUS"
val EOF = "EOF"*/

//para multilineas, tomamos el token ; y a lo que sigue le aplicamos la gramatica


object CompilerFirstTry{ 
	var variablef:Map[Char,Double] = Map()
	variablef += ('e' -> 2.718281828459045)
	variablef +=('p'->3.141592653589793)

	
	def main(args: Array[String]){ 
		
	  	val in = new Interpreter(Source.fromFile(args(0)).mkString)

	  	in.print()

	 }
}


class Token(tipo : String, valor: Char) {
	def print(){
	println(tipo + " " +valor)}
	def getType=tipo
	
	def getVal=valor

	override def toString = "sad"
	
}

class Interpreter(archivo: String){
	def print(){
		//println(archivo)
		val texto = archivo.split('\n').map(_.trim.filter(_ >= ' ')).mkString
		//println(texto)
		var lineas = texto.split(";")
		var lex = new Lexer(lineas)
	}

 }

 class Lexer(texto: Array[String])
 {


 	val variables = List('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z')
 	for(x <- texto)
 	{
 		var lineList = get_next_t(x) 		
		/*for(d<-lineList)
		{	
			d.print
		}*/

		var pars = new Parser(lineList)
 	}
 
 	
 	//var tokenList = new ListBuffer[Token]()
 	
	def get_next_t(s: String) =
	{	
		var lineList = new ListBuffer[Token]()
		var pos = 0
		if((s indexOf "var")== 0)
		{
			lineList += new Token("VAR", 'v')
			pos = pos+3
		}
		var lista = s.toList
		while(pos< lista.length)
 		{
	 		
			var c = lista(pos)
			
			if(c.isDigit)
			{
			
				lineList += new Token("DIGITO", c)

			}
			if(c=='.')
			{
				lineList  += new Token("PUNTO", c)
			}
			if(c=='(')
			{
				lineList  += new Token("OPEN", c)
			}
			if(c==')')
			{
				lineList  += new Token("CLOSE", c)
			}
			if(c == '+')
			{
			

				lineList  += new Token("SUMA", c)
				
			}
			if(c == '-')
			{
				
				lineList  += new Token("RESTA", c)
			}
			
			if(c == '*')
			{
				
				lineList  += new Token("MULTIPLICACION", c)
			}
			
			if(c == '/')
			{
			
				lineList  += new Token("DIVISION", c)
			}
			if(c == '^')
			{
			
				lineList  += new Token("POTENCIA",c)
			}

			if(c == '%')
			{
			
				lineList  += new Token("MODULO", c)
			}
			if(c == '=')
			{
				
				lineList += new Token("IGUALDAD", c)
			}

			for(d<-variables)
			{
				if(c==d)
				{

					
					lineList += new Token("LETRA",c)
					
				}
			}
			pos = pos + 1	
		}
		lineList
		

	}
 }

 class Parser(tokens: ListBuffer[Token])
 {
 	var result: Double = 0
 	var vari : Char = '0'
 	var tama = tokens.length
	var pos = 0;
	if(tokens(pos).getType =="VAR")
	{
		CompilerFirstTry.variablef += (tokens(pos+1).getVal -> 0)
		if(pos < tama)
		{pos = pos +1}
		
	}
	println(A)
	//println(result)
//	tokens(pos)
	def A =
	{
		println("ENtro a A")
		var x = false
		if(Y && P)
		{//println("A")
			x=true
		}
		else if(E)
		{//println("A")
			x=true
		}
		println("Sale de A")
		x
	}

	def P =
	{
		println("ENtro a P")
		var x = false
		if(tokens(pos).getVal == '=')
		{
		
		println(tokens(pos).getVal)
			if(pos < tama)
			{pos = pos +1}
			if(E){
			x =true}
			
		}
		println("Sale de P")
		x
	}

	def E:Boolean =
	{
		
		println("ENtro a E")
		var x = false
		if(X(0) && Q)
		{//println("E")
			x=true
		}else if(X(0))
		{//println("E")
			x=true
		}
		println("Sale de E")
		x
	}
	def Q = 
	{
		println("entre a Q")
		var x = false
		if(tokens(pos).getVal == '+')
		{
		println("Q")
		println(tokens(pos).getVal)
			if(pos < tama)
			{pos = pos +1}
			if(E)
			{x=true}
			}else if(tokens(pos).getVal == '-')
			{println("Q")
			println(tokens(pos).getVal)
				if(pos < tama)
				{pos = pos +1}
				if(E)
				{x=true}
			}
			println("Sale de Q")
		x
	}

	def X(times: Int):Boolean =
	{
		println("ENtro a X")
		var x = false
		if(Z(0))
		{//println("X1")
			x=true
		}else if(times < 2 && X(times+1) && R)
		{//println("X2")
			x=true
		}
		println("Sale de X")
		x
	}

	def R = 
	{
		println("ENtro a R")
		var x = false
		if(tokens(pos).getVal == '*')
		{//println("R")
		println(tokens(pos).getVal)
			if(pos < tama)
			{pos = pos +1}
			if(Z(0))
			{x=true}
			}else if(tokens(pos).getVal == '/' )
			{//println("R")
			println(tokens(pos).getVal)
				if(pos < tama)
				pos = pos +1
				if(Z(0))
				{x=true}
			}else if(tokens(pos).getVal== '%')
			{//println("R")
			println(tokens(pos).getVal)
				if(pos < tama)
				{pos = pos +1}
				if(Z(0))
				{x=true}
			}
			println("Sale de R")
			x
	}

	def Z(times:Int):Boolean = 
	{
		println("ENtro a Z")
		var x = false
		if(F)
		{println("Z1")
			x=true
		}else if(times<=2 && Z(times+1) && T)
		{println("Z2")
			x=true
		}
		println("Sale de Z")
		x
	}

	def T =
	{
		println("ENtro a T")
		var x = false
		if(tokens(pos).getVal == '^')
		{	//println("T")
		println(tokens(pos).getVal)
			if(pos < tama)
			{pos = pos +1}
			if(F)
			{x=true}
		}
		println("Sale de T")
		x
			
	}

	def F =
	{
		println("ENtro a F")
		var x = false
		if(N(0))
		{//println("F")
			x=true
		}else if(tokens(pos).getVal == '(')
		{//println("F")
		println(tokens(pos).getVal)
			if(pos < tama)
			{pos = pos +1}
			if(U)
			{x=true}
		}else if(Y)
		{//println("F")

			x=true
		}
		println("Sale de F")
		x
	}

	def U =
	{
		println("ENtro a U")
		var x = false
		if(E && tokens(pos).getVal == ')')
		{
		//println("U")
		println(tokens(pos).getVal)
			if(pos < tama)
			{pos = pos +1}
			x=true
		}
		println("Sale de U")
		x
	}

	def Y =
	{	
		println("ENtro a Y")
		var x = false
		if(tokens(pos).getType == "LETRA")
		{
			
			
			if(CompilerFirstTry.variablef.contains(tokens(pos).getVal))
			{
				x = true

			}
			else
			{
				println("Error Variable no definida")
				System.exit(1)
			}
			
			println(tokens(pos).getVal)
			if(pos < tama)
			{pos = pos +1}
			x = true
		}
		println("Sale de Y")
		x	
	}

	def N(times:Int):Boolean =
	{	
		println("ENtro a N")
		var x = false
		
		if(O)
		{
		
			x=true
		}else if(M(0) && tokens(pos).getVal == '.')
		{
		//println("N")
		println(tokens(pos).getVal)
			if(pos < tama)
			{pos = pos +1}
			x=true
		}else if(times<=2 && N(times+1) && tokens(pos).getType == "DIGITO")
		{
		//println("N")
		println(tokens(pos).getVal)
			if(pos < tama)
			{pos = pos +1}
			x=true
		}
		println("Sale de N")
		x
	}

	def O = 
	{
		println("entre a O " +tokens(pos).getVal)
	//println("ENtro a O")
		var x = false
		if(tokens(pos).getType == "DIGITO")
		{println("O")
		println(tokens(pos).getVal)
			if(pos < tama)
			{pos = pos +1}
			x=true
		}
		println("Sale de O")
		x

	}
	def M(times:Int):Boolean = 
	{
		println("ENtro a M")
		var x = false
		if(O)
		{
		//println("M")
			
			x=true
		}else if(times < 2 && M(times+1) &&tokens(pos).getType == "DIGITO" )
		{

		//println("M")
		println(tokens(pos).getVal)
			if(pos < tama)
			{pos = pos +1}
			x=true
		}
		println("Sale de M")
		x
	}
 }

 /*
A -> Y P | E
P -> = E
E -> X Q | X
Q -> + E | - E 
X -> X R | Z
R -> * Z | / Z | % Z
Z -> Z T | F
T-> ^ F
F -> N | ( U | Y
U -> E )
Y -> a | b | c
N -> N 0 | N 1 | N 2 | N 3 | N 4 | N 5 | N 6 | N 7 | N 8 | N 9 | O | M .
O -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
M -> M 0 | M 1 | M 2 | M 3 | M 4 | M 5 | M 6 | M 7 | M 8 | M 9 | O*/