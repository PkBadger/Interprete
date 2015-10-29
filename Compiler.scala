import scala.io.Source
import scala.util.control._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.math.pow

/*val INTEGER = "INTEGER"
val PLUS = "PLUS"
val MINUS = "MINUS"
val EOF = "EOF"*/

//para multilineas, tomamos el token ; y a lo que sigue le aplicamos la gramatica


object Compiler{ 
	var variablef:Map[Char,Double] = Map()
	variablef += ('e' -> 2.718281828459045)
	variablef +=('p'->3.141592653589793)

	
	def main(args: Array[String]){ 
		
	  	val in = new Interpreter(Source.fromFile(args(0)).mkString)

	  	in.print()

	 }
}


class Token(tipo : String, valor: String) {
	def print(){
	println(tipo + " " +valor)}
	def getType=tipo
	
	def getVal=valor

	override def toString = "sad"
	
}

class Interpreter(archivo: String){
	def print(){
		////println(archivo)
		val texto = archivo.split('\n').map(_.trim.filter(_ >= ' ')).mkString
		////println(texto)
		var lineas = texto.split(";")
		var lex = new Lexer(lineas)
	}

 }

 class Lexer(texto: Array[String])
 {


 	val variables = List('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z')
 	for(x <- texto)
 	{
 		var y = x.reverse
 		var lineList = get_next_t(y) 		
		/*for(d<-lineList)
		{	
			d.print
		}*/
		lineList += new Token("ENDL" ,"FINAL")
		var pars = new Parser(lineList)
 	}
 
 	
 	//var tokenList = new ListBuffer[Token]()
 	
	def get_next_t(s: String) =
	{	
		var lineList = new ListBuffer[Token]()
		var pos = 0
	
		var lista = s.toList
		
		while(pos< lista.length)
 		{
	 		////println("init pos " + pos + " Why" + lista(pos-1))

			var c = lista(pos)
			
			if(c.isDigit)
			{
				
				var numero = new StringBuilder
				var punto = false
				while(pos < lista.length &&lista(pos).isDigit)
				{
				numero += lista(pos)
				pos += 1
					if(!punto && pos < lista.length && lista(pos) == '.' )
					{
						numero += lista(pos)
						pos += 1
						punto = true
					}
					//println("pos " + pos + " ")

				}
				lineList += new Token("DIGITO", numero.toString)
			}
			if(c=='(')
			{
				lineList  += new Token("OPEN", c.toString)
				pos = pos + 1
			}
			if(c==')')
			{
				lineList  += new Token("CLOSE", c.toString)
				pos = pos + 1
			}
			if(c == '+')
			{
			

				lineList  += new Token("SUMA", c.toString)
				pos = pos + 1
				
			}
			if(c == '-')
			{
				
				lineList  += new Token("RESTA", c.toString)
				pos = pos + 1
			}
			
			if(c == '*')
			{
				
				lineList  += new Token("MULTIPLICACION", c.toString)
				pos = pos + 1
			}
			
			if(c == '/')
			{
			
				lineList  += new Token("DIVISION", c.toString)
				pos = pos + 1
			}
			if(c == '^')
			{
			
				lineList  += new Token("POTENCIA",c.toString)
				pos = pos + 1
			}

			if(c == '%')
			{
			
				lineList  += new Token("MODULO", c.toString)
				pos = pos + 1
			}
			if(c == '=')
			{
				
				lineList += new Token("IGUALDAD", c.toString)
				pos = pos + 1
			}

			for(d<-variables)
			{
				if(c==d)
				{

					
					lineList += new Token("LETRA",c.toString)
					pos = pos + 1
					
				}

			}
			if(c == ' ')
			{
				pos = pos + 1
			}

		
				
		}

		lineList
		

	}
 }

 class Parser(tokens: ListBuffer[Token])
 {
 	val a = A(0)
	//println(a._1)
	if(!a._4)
	{println(a._3)}
	
	
	
	def A(pos1 : Int) =
	{
		//println("ENtra a A")
		var pos = pos1
		var x = false //veridica si la linea es correcta
		var asig = false
		var result:Double = 0 //Resultados
		
		val e = E(pos)
		if(e._1 && tokens(e._2).getVal == "=")
		{
			
			val y = Y(e._2 + 1) 
			if(y._1)
			{
				x = true
				Compiler.variablef +=(y._3->e._3) //resultado devuelto por e

				asig = true
				pos = y._2 //posicion devuelta por e
			}
		}
		else if(e._1)
		{
			var e = E(pos)
			if(e._1)
			{
				x= true
				result = e._3
				pos = e._2 //posicion devuelta por e
			}
		}
		//println("Sale de A " + result)
		(x,pos,result,asig)

	}

	def E(pos1: Int):(Boolean,Int,Double) =
	{
		//println("ENtra a E")
		var pos = pos1
		var q = false
		var result:Double = 0
		val x = X(pos)
		if(x._1) //verifica la parte de la X
		{
			if(tokens(x._2).getVal == "+")
			{
				val e = E(x._2 +1)
				if(e._1)
				{
					pos = e._2
					result = x._3 + e._3
					q = true
				}
			} else if(tokens(x._2).getVal == "-"){
				val e = E(x._2 +1)
				if(e._1)
				{
					pos = e._2
					result =e._3 -  x._3
					println(x._3 +" - " + e._3 +" = " + result)
					q = true
				}
			}else
			{
				result = x._3
				pos = x._2
				q = true
			}
			
		}
		//println("Sale de E " + result)
		(q,pos,result)
	}
	
	def X(pos1: Int):(Boolean,Int,Double)  =
	{
		//println("ENtra a X")
		var pos = pos1
		var x = false
		var result:Double = 0
		val z = Z(pos)
		if(z._1)
		{
			val xp = XP(z._2,z)
			if(xp._1)
			{
				x =true
				pos = xp._2
				result = xp._3
			}
		}
		//println("Sale de X " + result)
		(x,pos,result)	
	}
	def XP(pos1: Int, za: (Boolean,Int,Double) ):(Boolean,Int,Double)  = 
	{
		//println("ENtra a XP")
		var pos = pos1
		var x = false
		var result:Double = 1
		if(tokens(pos).getVal == "*")
		{
			val z = Z(pos+1)
			if(z._1)
			{
				val xp = XP(z._2,z)
				if(xp._1)
				{
					x = true
					result = za._3 * xp._3
					pos =xp._2
				}
			}

		}else if(tokens(pos).getVal == "/"){
			val z = Z(pos+1)
			if(z._1)
			{
				val xp = XP(z._2,z)
				if(xp._1)
				{
					x = true
					result = xp._3 / za._3
					pos =xp._2
				}
			}

		

		}else if(tokens(pos).getVal == "%")
		{
			val z = Z(pos+1)
			if(z._1)
			{
				val xp = XP(z._2,z)
				if(xp._1)
				{
					x = true
					result = xp._3 % za._3 
					pos =xp._2
				}
			}
		}else
		{
			x=true
			result = za._3
		}
		//println("Sale de XP " + result)
		(x,pos,result)
	}

	def Z(pos1: Int):(Boolean,Int,Double) = 
	{
		//println("ENtra a Z")
		var pos = pos1
		var x = false
		var result:Double = 0
		val f = F(pos)
		if(f._1)
		{
			val zp = ZP(f._2,f)
			if(zp._1)
			{
				x = true
				result = zp._3
				pos = zp._2
			}
		}
//println("Sale de Z " + result)
		(x,pos,result)
	}

	def ZP(pos1:Int,fa:(Boolean,Int,Double) ):(Boolean,Int,Double) =
	{
		//println("ENtra a ZP")
		var pos = pos1
		var x = false
		var result:Double = 0
		if(tokens(pos).getVal == "^")
		{
			val f = F(pos+1)
			if(f._1)
			{
				val zp = ZP(f._2,f)
				if(zp._1)
				{
					x = true
					result = pow(zp._3,fa._3)
					pos =zp._2
				}
			}

		} else
		{
			x = true
			result = fa._3
		}
		//println("Sale de ZP " + result)
		(x,pos,result)

	}

	def F(pos1: Int)=
	{
		//println("ENtra a F")
		var pos = pos1
		var x = false
		var result:Double = 0
		val n = N(pos)
		if(n._1)
		{
			result = n._3
			x = true
			pos = n._2
		} else if(tokens(pos).getVal == ")")
		{
			val u = U(pos+1)
			if(u._1)
			{
				result = u._3
				pos = u._2
				x = true
			}
		}else {
			val y = Y(pos)
			if(y._1)
			{
				
				pos = y._2
				x = true
				if(Compiler.variablef.contains(y._3))
				{
					
					result = Compiler.variablef(y._3)

				}else
				{	println("La variable no fue inicializada")
					System.exit(1)}
				
				
			}	
		}
		//println("Sale de F " + result)
		(x,pos,result)
	}

	def U(pos1: Int)=
	{
		//println("ENtra a U")
		var pos = pos1
		var x = false
		var result:Double = 0
		val e = E(pos)
		if(e._1)
		{
			if(tokens(e._2).getVal == "(")
			{
				x = true
				result = e._3
				pos = e._2 + 1
			}
		}
		//println("Sale de U " + result)
		(x,pos,result)
	}

	def Y(pos1: Int) =
	{	
		//println("ENtra a Y")
		var pos = pos1
		var x = false
		var result:Double = 0
		var letra:Char = ' '
		
		if(tokens(pos).getType == "LETRA")
		{

			//print("Soy letra")
			
				x = true
				letra = (tokens(pos).getVal.toList)(0)
				pos = pos +1

			//println(tokens(pos).getVal)
			
			
		}
		//println("Sale de Y " )
		(x,pos,letra)
	}

	def N(pos1: Int):(Boolean,Int,Double)  =
	{	
		//println("ENtra a N")
		var pos = pos1
		var x = false
		var result:Double = 0
		if(tokens(pos).getType =="DIGITO")
		{
			
			result = tokens(pos).getVal.toDouble
			pos = pos + 1
			x = true

		}
		//println("Sale de N " + result)
		(x,pos,result)
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


 /*
A -> Y = E | E
E -> X + E | X - E | X

X -> X * Z | X / Z | X % Z | Z
Z -> Z ^ F | F
F -> N | ( E ) | Y
Y -> a | b | c
N -> N 0 | N 1 | N 2 | N 3 | N 4 | N 5 | N 6 | N 7 | N 8 | N 9 | O | M .
O -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
M -> M 0 | M 1 | M 2 | M 3 | M 4 | M 5 | M 6 | M 7 | M 8 | M 9 | O*/