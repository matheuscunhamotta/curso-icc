PROGRAM Relatorio_da_Area_de_Uma_Circunferencia
IMPLICIT NONE

INTEGER                 :: i, k, j, n_triang
REAL                    :: triangulo(100,4)
CHARACTER (len=10)      :: nome_triang(100), tipo(100)

!
! -------------- Modulo: Obter dados
WRITE (*,*) "Informe a quantidade de triangulos: (NO MAXIMO 100)"
READ  (*,*) n_triang
WRITE (*,*)

WRITE (*,*)
WRITE (*,*) "ATENCAO: No calculo da AREA, assumimos que o LADO 1 e' a medida da &
  & base e que o LADO 2 e' a medida da altura!"
WRITE (*,*)
WRITE (*,*)

DO i = 1, n_triang
  WRITE (*,*) "Informe o nome do triangulo", i, " :"
  READ  (*,*) nome_triang(i)
  DO k = 1, 3
    WRITE (*,*) "Informe a medida do lado ", k, " do triangulo ", i
    READ  (*,*) triangulo(i,k)
  END DO

  WRITE (*,*)
  WRITE (*,*)

  ! Verificando se os dados inseridos valem para a definicao de triangulo
  ! E' necessario que a expressao abaixo seja Verdadeira para os 3 lados
  ! |b - c| < a < b + c

  ! A falta de tempo levou-me a usar a horrorosa estrutura abaixo

  ! Primeiro caso ---
  IF ( .NOT. &
       & ((triangulo(i, 2) - triangulo(i, 3))*(-1)) < triangulo(i, 1) &
       & .AND.                                                        &
       & triangulo(i, 1) < (triangulo(i, 2) + triangulo(i, 3))   )    &
       & THEN
    WRITE (*,*)
    WRITE (*,*)
    WRITE (*,*) "As medidas fornecidas nao sao validas!"
    WRITE (*,*) "Por favor, insira medidas validas para triangulo."
    DO k = 1, 3
      WRITE (*,*) "Informe a medida do lado ", k, " do triangulo ", i
      READ  (*,*) triangulo(i,k)
    END DO
  END IF

  ! Segundo caso ---
  IF ( .NOT. &
       & ((triangulo(i, 3) - triangulo(i, 1))*(-1)) < triangulo(i, 2) &
       & .AND.                                                        &
       & triangulo(i, 2) < (triangulo(i, 3) + triangulo(i, 1))   )    &
       & THEN
    WRITE (*,*)
    WRITE (*,*)
    WRITE (*,*) "As medidas fornecidas nao sao validas!"
    WRITE (*,*) "Por favor, insira medidas validas para triangulo."
    DO k = 1, 3
      WRITE (*,*) "Informe a medida do lado ", k, " do triangulo ", i
      READ  (*,*) triangulo(i,k)
    END DO
  END IF

  ! Terceiro caso ---
  IF ( .NOT. &
       & ((triangulo(i, 1) - triangulo(i, 2))*(-1)) < triangulo(i, 1) &
       & .AND.                                                        &
       & triangulo(i, 3) < (triangulo(i, 1) + triangulo(i, 2))   )    &
       & THEN
    WRITE (*,*)
    WRITE (*,*)
    WRITE (*,*) "As medidas fornecidas nao sao validas!"
    WRITE (*,*) "Por favor, insira medidas validas para triangulo."
    DO k = 1, 3
      WRITE (*,*) "Informe a medida do lado ", k, " do triangulo ", i
      READ  (*,*) triangulo(i,k)
    END DO
  END IF

END DO

!
! -------------- Modulo: Calcular a area do triangulo
! A questao nao solicita o estudo de casos como o do calculo da area do
! triangulo equilatero, portanto:
DO i = 1, n_triang
  triangulo(i,4) = (triangulo(i,1) * triangulo(i,2)) / 2
END DO

!
! -------------- Modulo: Verificar os tipos de triangulo
DO i = 1, n_triang
  IF ( &
       & triangulo(i, 1) == triangulo(i, 2) &
       & .AND.                              &
       & triangulo(i, 2) == triangulo(i, 3)) THEN
    tipo(i) = "Equilatero"
  ELSE
    IF ( &
         & triangulo(i, 1) == triangulo(i, 2) &
         & .OR.                               &
         & triangulo(i, 1) == triangulo(i, 3)) THEN
      tipo(i) = "Isosceles"
    ELSE
      tipo(i) = "Escaleno"
    END IF
  END IF
END DO

!
! -------------- Modulo: Exibir Relatorio
WRITE (*,*)
WRITE (*,*)

WRITE (*,*) "=========================================================="
WRITE (*,*) "============ RELATORIO DA AREA DO TRIANGULO =============="
WRITE (*,*) "=========================================================="
WRITE (*,*) "===== TRIANGULO ====  BASE  === ALTURA ==   AREA   ======="
DO i = 1, n_triang
  WRITE (*,*) "=                                                        ="
  WRITE (*,50) " = ", i, triangulo(i,1), triangulo(i,2), triangulo(i,4), "  ="
END DO
WRITE (*,*) "=                                                        ="
WRITE (*,*) "=========================================================="

WRITE (*,*)
WRITE (*,*)

WRITE (*,*) "==============================================================================="
WRITE (*,*) "=================== RELATORIO DE INFORMACOES DO TRIANGULO ====================="
WRITE (*,*) "==============================================================================="
WRITE (*,*) "===   NOME  =====    L1   =====    L2    =====    L3    =====    TIPO    ======"
DO i = 1, n_triang
  WRITE (*,*) "=                                                                             ="
  !             N/A,      Nome      ,     Lado 1    ,     Lado 2    ,     Lado 3    ,  Tipo     N/A
  !            AAAAA, AAAAAAAAAAAAAA, FFFFFFFFFFFFFF, FFFFFFFFFFFFFF, FFFFFFFFFFFFFF, AAAAAAA, AAAAA
  WRITE (*,55) " = ", nome_triang(i), triangulo(i,1), triangulo(i,2), triangulo(i,3), tipo(i), " ="
END DO
WRITE (*,*) "=                                                                             ="
WRITE (*,*) "==============================================================================="

50 FORMAT (A, 8X, I1, 10X, F6.2, 4X, F6.2, 7X, F6.2, 5x, A)
55 FORMAT (A, 4X, A6, F14.2, F14.2, 2X, F14.2, 6X, A10, 5X, A)
END PROGRAM
