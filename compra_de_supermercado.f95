PROGRAM Relatorio_da_Compra_de_Supermercado
IMPLICIT NONE

CHARACTER (len=10) :: n_prod(100)
INTEGER            :: i, unitarios
REAL               :: produtos(100, 4), total = 0
REAL               :: taxa(100), desconto(100)

CALL entrada(unitarios, n_prod, produtos)
CALL calcula(unitarios, produtos, total, taxa, desconto)
CALL saida(unitarios, n_prod, produtos, total, desconto)

END PROGRAM

! Subrotina entrada
SUBROUTINE entrada(unitarios, n_prod, produtos)

CHARACTER (len=10) :: n_prod(100)
INTEGER            :: i, unitarios
REAL               :: produtos(100, 4)

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
END SUBROUTINE

! Subrotina calcula
SUBROUTINE calcula(unitarios, produtos, total, taxa, desconto)

INTEGER            :: i, unitarios
REAL               :: produtos(100, 4)
REAL               :: taxa(100), desconto(100)

! Calculando a taxa de desconto
DO i = 1, unitarios
  IF (produtos(i, 1) <= 5) THEN
    taxa(i) = 0.05
  ELSE IF ((produtos(i, 1) > 5) .AND. (produtos(i, 1) <= 10)) THEN 
    taxa(i) = 0.15
  ELSE
    IF ((produtos(i, 1) > 10) .AND. (produtos(i, 1) <= 15)) THEN
      taxa(i) = 0.20
    ELSE
      taxa(i) = 0.30
    END IF
  END IF
END DO

! Calculando o custo total do produto
DO i = 1, unitarios
  produtos(i, 3) = produtos(i, 1) * produtos(i, 2)
END DO

! Calculando o valor do desconto
DO i = 1, unitarios
  desconto(i) = produtos(i, 1) * taxa(i)
END DO

! Calculando o valor total do produto com o desconto
DO i = 1, unitarios
  produtos(i, 4) = produtos(i, 3) - desconto(i)
END DO

! Calculando o valor total da compra
DO i = 1, unitarios
  total = produtos(i, 4) + total
END DO
END SUBROUTINE

! Subrotina saida
SUBROUTINE saida(unitarios, n_prod, produtos, total, desconto)

CHARACTER (len=10) :: n_prod(100)
INTEGER            :: i, unitarios
REAL               :: produtos(100, 4), total
REAL               :: desconto(100)

WRITE (*,*)
WRITE (*,*)

WRITE (*,*) "====================================================&
        &==========================="
WRITE (*,*) "================== RELATORIO DA COMPRA DE SUPERMERCA&
        &DO ========================"
WRITE (*,*) "====================================================&
        &==========================="
WRITE (*,*) "==== PRODUTO == QUANTIDADE == PRECO == VALOR == DESC&
        &ONTO == VALOR C.DESCONTO =="
DO i = 1, unitarios
  WRITE (*,*) "=                                                 &
        &                              ="
  WRITE (*,50) " = ", n_prod(i), produtos(i, 1), produtos(i, 2), &
        &produtos(i, 3), desconto(i), produtos(i, 4), "  ="
END DO
WRITE (*,*) "____________________________________________________&
        &___________________________"
WRITE (*,*)
WRITE (*,51) " =                                             TOTA&
        &L:    ", total, "        ="
WRITE (*,*) "=                                                   &
        &                          ="
WRITE (*,*) "====================================================&
        &==========================="

50 FORMAT (A, 3x, A6, 4X, F6.2, 7X, F6.2, 4X, F6.2, 2X, F6.2, 7X, F6.2, 10x, A)
51 FORMAT (A, 3x, F6.2, 4x, A)
END SUBROUTINE
