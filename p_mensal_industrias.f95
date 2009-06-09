PROGRAM Producao_Mensal_de_3_Industrias
IMPLICIT NONE

INTEGER :: i, j, k, abaixo_media(31)
REAL	:: industrias(7, 3), producao(3), media_total = 0

! Pega os valores
    DO i = 1, 3
      WRITE (*,*)
      DO j = 1, 7
        WRITE (*,*) "Insira o valor de producao do dia ", j, " da empresa ", i, ":"
        READ  (*,*) industrias(j, i)
      END DO
    END DO
!---------

! Pega a soma da producao diaria
    producao(1:3) = 0
    DO i = 1, 3
      DO j = 1, 7
        producao(i) = industrias(j,i) + producao(i)
      END DO
    END DO
!---------

! Calcula a media
    DO i = 1, 3
      producao(i) = producao(i)/7
    END DO
!---------

! ===========

! Cacula a media total das 3 empresas
DO i = 1, 3
  media_total = (producao(i) + media_total)
END DO
media_total = media_total / 3

abaixo_media = 0

! Verifica quais dias estao abaixo da media
DO i = 1, 3
  DO j = 1, 7
    IF (industrias(j,i) < producao(i)) THEN
      abaixo_media(i) = abaixo_media(i) + 1
    END IF
  END DO
END DO

WRITE (*,*) "========================================================="
WRITE (*,*) "===== RELATORIO DA PRODUCAO MENSAL DE 3 EMPRESAS ========"
WRITE (*,*) "========================================================="
WRITE (*,*) "== DIA ====  IND.1 ====  IND.2 ====  IND.3 =============="
DO j = 1, 7
  WRITE (*,5) " == ", j, (industrias(j, k), k = 1, 3), "              =="
END DO
WRITE (*,*) "==-----------------------------------------------------=="
WRITE (*,*) "== TOTAL : ", media_total, "                           =="
WRITE (*,*) "==-----------------------------------------------------=="
DO i = 1, 3
  WRITE (*,6) " == Dias abaixo da media da industria ", i, " : ", abaixo_media(i), "            =="
END DO
WRITE (*,*) "========================================================="

5 FORMAT (A, I2, 3(F12.2), A)
6 FORMAT (A, I1, A, I2, A)
END PROGRAM
