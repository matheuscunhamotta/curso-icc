PROGRAM Relatorio_da_Area_de_Uma_Circunferencia
IMPLICIT NONE

INTEGER	:: i, n_circ
REAL	:: circunferencia(100,2), PI = 3.1415926535897932384626433832795

WRITE (*,*) "Informe a quantidade de circunferencias: (NO MAXIMO 100)"
READ  (*,*) n_circ
WRITE (*,*)

DO i = 1, n_circ
  WRITE (*,*) "Qual o raio da circunferencia", i, " ?"
  READ  (*,*) circunferencia(i,1)
END DO

DO i = 1, n_circ
  circunferencia(i,2) = PI*(circunferencia(i,1)**2)
END DO

WRITE (*,*)
WRITE (*,*)

WRITE (*,*) "=========================================================="
WRITE (*,*) "======== RELATORIO DA AREA DA CIRCUNFERENCIA ============="
WRITE (*,*) "=========================================================="
WRITE (*,*) "====    RAIO    ========================    AREA    ======"
DO i = 1, n_circ
  WRITE (*,*) "=                                                        ="
  WRITE (*,*) "= ", circunferencia(i,1), "         |          ", circunferencia(i,2), "   ="
  WRITE (*,*) "=                                                        ="
END DO
WRITE (*,*) "=========================================================="

END PROGRAM
