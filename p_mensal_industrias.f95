PROGRAM Producao_Mensal_de_3_Industrias
IMPLICIT NONE

INTEGER :: i, j, abaixo_media(31)
REAL    :: industrias(7, 3), producao(3), media_total = 0

CALL entrada(industrias)
CALL calcula(abaixo_media, industrias, producao, media_total)
CALL saida(abaixo_media, industrias, media_total)

END PROGRAM

! Subrotina entrada
SUBROUTINE entrada(industrias)

INTEGER :: i, j
REAL    :: industrias(7, 3)

WRITE (*,*)
WRITE (*,*) "Atualmente esse programa calcula a producao de 3 industrias &
&num periodo de 7 dias"
WRITE (*,*)

DO i = 1, 3
  WRITE (*,*)
  DO j = 1, 7
    WRITE (*,*) "Insira o valor de producao do dia ", j, " da&
      & empresa ", i, ":"
    READ  (*,*) industrias(j, i)
  END DO
END DO
END SUBROUTINE

! Subrotina calcula
SUBROUTINE calcula(abaixo_media, industrias, producao, media_total)

INTEGER :: i, j, abaixo_media(31)
REAL    :: industrias(7, 3), producao(3), media_total

! Pega a soma da producao diaria
producao(1:3) = 0
DO i = 1, 3
  DO j = 1, 7
    producao(i) = industrias(j,i) + producao(i)
  END DO
END DO

! Calcula a media
DO i = 1, 3
  producao(i) = producao(i)/7
END DO

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
END SUBROUTINE

! Subrotina saida
SUBROUTINE saida(abaixo_media, industrias, media_total)

INTEGER :: i, j, abaixo_media(31)
REAL    :: industrias(7, 3), media_total

WRITE (*,*) "========================================================="
WRITE (*,*) "===== RELATORIO DA PRODUCAO MENSAL DE 3 EMPRESAS ========"
WRITE (*,*) "========================================================="
WRITE (*,*) "== DIA ====  IND.1 ====  IND.2 ====  IND.3 =============="
DO i = 1, 7
  WRITE (*,5) " == ", i, (industrias(i, k), k = 1, 3), "              =="
END DO
WRITE (*,*) "==-----------------------------------------------------=="
WRITE (*,*) "== TOTAL : ", media_total, "                           =="
WRITE (*,*) "==-----------------------------------------------------=="
DO i = 1, 3
  WRITE (*,6) " == Dias abaixo da media da industria ", i, " : ",&
        & abaixo_media(i), "            =="
END DO
WRITE (*,*) "========================================================="

5 FORMAT (A, I2, 3(F12.2), A)
6 FORMAT (A, I1, A, I2, A)
END SUBROUTINE
