package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    println(" ")
    println( "== Perfectly balanced balanced, as all things should be ==")
    println("Balance of " + "(Ta certo)".toList)
    println("RESPOSTA: " + theCoolerBalance("(Ta certo)".toList))
    println(" ")
    println("Balance of " + "(Ta ) errado(".toList)
    println("RESPOSTA: " + theCoolerBalance("(Ta ) errado(".toList))
    println(" ")
    println("Balance of " + "Ta ) errado(".toList)
    println("RESPOSTA: " + theCoolerBalance("Ta ) errado(".toList))
    println("===========================================================")
    println(" ")

  }

  /**
   * Exercise 1
   */
//  Cria uma linha apartir da linha anterior de acordo com as regras do triangulo de pascal
    def pascal(c: Int, r: Int):Int = {
      if (c < 0 || r < 0 || c > r) throw new IllegalArgumentException("Número de linhas ou colunas inválido") else
      if(r == 0 || r == 1) 1 else {
        def iterator(linhaAnterior: List[Int], nLinhaAtual: Int):Int={
          var novaLinha = List[Int]()
          for(n <- 0 to linhaAnterior.length - 2 ){
            novaLinha = novaLinha :+ (linhaAnterior(n) + linhaAnterior(n+1))
          }
          novaLinha = 1 :: novaLinha
          novaLinha = novaLinha :+ 1

          if(nLinhaAtual == r) novaLinha(c) else iterator(novaLinha, nLinhaAtual + 1)
        }
        iterator(List(1, 1), 2)
      }
    }

//    Exemplo que achei na internet depois de fazer o meu, achei genial
    def theCoolerPascal(c: Int, r: Int): Int = {
      if (c < 0 || r < 0 || c > r) throw new IllegalArgumentException("Column and row numbers must be 0 or greater. Column length must be lower than row length") else{
        if (c == 0 || c == r) 1 else {
          theCoolerPascal(c - 1, r - 1) + theCoolerPascal(c, r - 1)
        }
      }
    }
  
  /**
   * Exercise 2
   */
//  Utiliza a lógica de fechamentos para aberturas
//  Pra cada abertura deve ter um fechamento, se pra todos os fechamentos tem uma abertura,
//  E os números de aberturas e fechamentos são iguais, está balanceado
    def balance(chars: List[Char]):Boolean = {
      def assertBalance(aux: List[Char], nFechamentos: Int, nAberturas: Int): Boolean = {
        if(aux.isEmpty) nFechamentos == nAberturas else aux.head match {
          case ')' => if(nFechamentos + 1 > nAberturas) false else assertBalance(aux.tail, nFechamentos + 1, nAberturas)
          case '(' => {
            if (procuraFechamento(aux.tail)) assertBalance(aux.tail, nFechamentos, nAberturas + 1)
            else false
          }
          case _ => assertBalance(aux.tail, nFechamentos, nAberturas)
        }
      }

      def procuraFechamento(aux: List[Char]): Boolean = {
        if(aux.isEmpty) false else {
          if(aux.head == ')') true
          else if(aux.tail.nonEmpty) procuraFechamento(aux.tail)
          else false
        }
      }

      assertBalance(chars, 0, 0)
    }

//  Utiliza a lógica de equilíbrio -> fechamentos = -1, aberturas = +1
//  o equilibrio deve estar em 0 e nunca ser negativo
    def theCoolerBalance(chars: List[Char]):Boolean = {
      def countBalance(aux: List[Char], balance: Int): Boolean = if(aux.isEmpty) balance == 0 else aux.head match{
        case '(' => countBalance(aux.tail, balance + 1)
        case ')' => if((balance - 1) < 0) false else countBalance(aux.tail, balance - 1)
        case  _  => countBalance(aux.tail, balance)
      }
      countBalance(chars, 0)
    }
  
  /**
   * Exercise 3
   */
  /**
    * "se as moedas foram demais para quitar a dívida, ou não teve moedas o suficiente, esse não é um jeito de quitar a dívida"
    * "se a dívida for quitada, essa é uma maneira de quitar a dívida"
    * "senão continuemos andando, utilize a primeira moeda para tentar quitar a dívida e (+) tente com a próxima moeda"
    */
    def countChange(money: Int, coins: List[Int]): Int = {
      if(money == 0) 1
      else if(money < 0 || coins.isEmpty) 0
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }

    def sum(f: Int => Int, a: Int, b: Int): Int = {
      def loop(a: Int, acc: Int): Int = {
        if (a > b) acc
        else loop(a + 1, acc + f(a))
      }
      loop(a, 0)
    }
  }
