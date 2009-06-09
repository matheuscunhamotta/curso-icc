PROGRAM N_Fatoriais
IMPLICIT NONE

INTEGER	:: i, j, n_fatores
REAL	:: fatorial(100,2) = 1

WRITE (*,*) "Quantos 'fatoriais' deseja calcular? (NO MAXIMO 100)"
READ  (*,*) n_fatores
WRITE (*,*)

DO i = 1, n_fatores
  WRITE (*,*) "Informe o valor do fatorial ", i, " :"
  READ  (*,*) fatorial(i, 1)
END DO

DO i = 1, n_fatores
  DO j = 1, fatorial(i, 1)
    fatorial(i, 2) = fatorial(i, 2) * j
  END DO
END DO

WRITE (*,*)
WRITE (*,*)

WRITE (*,*) "=========================================================="
WRITE (*,*) "============== RELATORIO DO FATORIAL ====================="
WRITE (*,*) "=========================================================="
WRITE (*,*) "===== ID FATORIAL  ==================== FATORIAL ========="
DO i = 1, n_fatores
  WRITE (*,*) "=                                                        ="
  WRITE (*,50) " = ", fatorial(i, 1), fatorial(i, 2), "  ="
END DO
WRITE (*,*) "=                                                        ="
WRITE (*,*) "=========================================================="

50 FORMAT (A, 6x, F6.2, 28x, F6.2, 7x, A)
END PROGRAM
