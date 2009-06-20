PROGRAM N_Fatoriais
IMPLICIT NONE

INTEGER :: i, j, n_fatores
INTEGER *8 :: fatorial(100,2) = 1

CALL entrada(n_fatores, fatorial)
CALL calcula(n_fatores, fatorial)
CALL saida(n_fatores, fatorial)

END PROGRAM

! Subrotina entrada
SUBROUTINE entrada(n_fatores, fatorial)

INTEGER :: i, n_fatores
INTEGER *8 :: fatorial(100,2)

WRITE (*,*) "Quantos 'fatoriais' deseja calcular? (NO MAXIMO 100)"
READ  (*,*) n_fatores
WRITE (*,*)

DO i = 1, n_fatores
  WRITE (*,*) "Informe o valor do fatorial ", i, " :"
  READ  (*,*) fatorial(i, 1)
END DO
END SUBROUTINE

! Subrotina calcula
SUBROUTINE calcula(n_fatores, fatorial)

INTEGER :: i, j, n_fatores
INTEGER *8 :: fatorial(100,2)

DO i = 1, n_fatores
  DO j = 1, fatorial(i, 1)
    fatorial(i, 2) = fatorial(i, 2) * j
  END DO
END DO
END SUBROUTINE

! Subrotina saida
SUBROUTINE saida(n_fatores, fatorial)

INTEGER :: i, n_fatores
INTEGER *8 :: fatorial(100,2)

WRITE (*,*)
WRITE (*,*)

WRITE (*,*) "=========================================================="
WRITE (*,*) "============== RELATORIO DO FATORIAL ====================="
WRITE (*,*) "=========================================================="
WRITE (*,*) "============= ID FATORIAL  ============ FATORIAL ========="
DO i = 1, n_fatores
  WRITE (*,*) "=                                                        ="
  WRITE (*,*) "= ", fatorial(i, 1), "       ", fatorial(i, 2), "    ="
END DO
WRITE (*,*) "=                                                        ="
WRITE (*,*) "=========================================================="
END SUBROUTINE
