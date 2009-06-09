PROGRAM Relatorio_da_Compra_de_Supermercado
IMPLICIT NONE

CHARACTER (len=10) :: n_prod(100)
INTEGER	           :: i, unitarios
REAL	           :: produtos(100, 3), total = 0

WRITE (*,*) "Informe a quantidade de produtos comprados:"
READ  (*,*) unitarios
WRITE (*,*)

DO i = 1, unitarios
  WRITE (*,*) "Informe o nome do produto ", i, " :"
  READ  (*,*) n_prod(i)
  WRITE (*,*) "Quantas unidades de ", n_prod(i), " comprou?"
  READ  (*,*) produtos(i, 1)
  WRITE (*,*) "Qual o preco unitario de ", n_prod(i), " :"
  READ  (*,*) produtos(i, 2)
END DO

DO i = 1, unitarios
  produtos(i, 3) = produtos(i, 1) * produtos(i, 2)
END DO

DO i = 1, unitarios
  total = produtos(i, 3) + total
END DO

WRITE (*,*)
WRITE (*,*)

WRITE (*,*) "==============================================================================="
WRITE (*,*) "================== RELATORIO DA COMPRA DE SUPERMERCADO ========================"
WRITE (*,*) "==============================================================================="
WRITE (*,*) "===== NOME ==== QUANTIDADE === PRECO UNITARIO == PRECO PRODUTO ===== TOTAL ===="
DO i = 1, unitarios
  WRITE (*,*) "=                                                                             ="
  WRITE (*,50) " = ", n_prod(i), produtos(i, 1), produtos(i, 2), produtos(i, 3), "  ="
END DO
WRITE (*,*) "_______________________________________________________________________________"
WRITE (*,*)
WRITE (*,51) " =                                                             | ", total, "="
WRITE (*,*) "=                                                                             ="
WRITE (*,*) "==============================================================================="

50 FORMAT (A, 4x, A6, 5X, F6.2, 8X, F6.2, 13X, F6.2, 20x, A)
51 FORMAT (A, 4x, F6.2, 4x, A)

END PROGRAM
