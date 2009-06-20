PROGRAM Relatorio_da_Area_dos_Triangulos
IMPLICIT NONE

INTEGER                 :: i, k, j, n_triang
REAL                    :: triangulo(100,4)
CHARACTER (len=10)      :: nome_triang(100), tipo(100)

CALL entrada(n_triang, triangulo, nome_triang)
CALL calculo(n_triang, triangulo, tipo)
CALL saida(n_triang, triangulo, tipo, nome_triang)

END PROGRAM

! Subrotina de entrada
SUBROUTINE entrada(n_triang, triangulo, nome_triang)

INTEGER                 :: i, k, n_triang
REAL                    :: triangulo(100,4)
CHARACTER (len=10)      :: nome_triang(100)

! -------------- Modulo: Obter dados
WRITE (*,*) "Informe a quantidade de triangulos: (NO MAXIMO 100)"
READ  (*,*) n_triang
WRITE (*,*)

WRITE (*,*)
WRITE (*,*) "ATENCAO: No calculo da AREA, assumimos que o LADO 1 &
        &e' a medida da base e que o LADO 2 e' a medida da altura!"
WRITE (*,*)
WRITE (*,*)

10 DO i = 1, n_triang
  WRITE (*,*) "Informe o nome do triangulo", i, " :"
  READ  (*,*) nome_triang(i)
  DO k = 1, 3
    WRITE (*,*) "Informe a medida do lado ", k, " do triangulo ", i
    READ  (*,*) triangulo(i,k)
  END DO
  WRITE (*,*)
  WRITE (*,*)
  ! Verica se os valores atribuidos valem para a definicao de triangulo
  IF ( &
       & .NOT.                                                        &
       & ((triangulo(i, 2) - triangulo(i, 3))*(-1)) < triangulo(i, 1) &
       & .AND.                                                        &
       & triangulo(i, 1) < (triangulo(i, 2) + triangulo(i, 3))        &
       & .OR.                                                         &
       & .NOT.                                                        &
       & ((triangulo(i, 3) - triangulo(i, 1))*(-1)) < triangulo(i, 2) &
       & .AND.                                                        &
       & triangulo(i, 2) < (triangulo(i, 3) + triangulo(i, 1))        &
       & .OR.                                                         &
       & .NOT.                                                        &
       & ((triangulo(i, 1) - triangulo(i, 2))*(-1)) < triangulo(i, 1) &
       & .AND.                                                        &
       & triangulo(i, 3) < (triangulo(i, 1) + triangulo(i, 2)))       &
       & THEN
    WRITE (*,*)
    WRITE (*,*) "As medidas fornecidas nao sao validas!"
    WRITE (*,*) "Por favor, insira medidas validas para triangulo."
    WRITE (*,*)
    GOTO 10
  END IF
END DO
END SUBROUTINE

! Subrotina calculo
SUBROUTINE calculo(n_triang, triangulo, tipo)

INTEGER                 :: i, n_triang
REAL                    :: triangulo(100,4)
CHARACTER (len=10)      :: tipo(100)

! -------------- Modulo: Calcular a area do triangulo
! Assumindo que LADO 1 sempre sera' a medida da altura, temos:
DO i = 1, n_triang
  triangulo(i,4) = (triangulo(i,1) * triangulo(i,2)) / 2
END DO

! -------------- Modulo: Verificar os tipos de triangulo
DO i = 1, n_triang
  IF ((triangulo(i, 1) == triangulo(i, 2)) &
       & .AND. (triangulo(i, 2) == triangulo(i, 3))) THEN
    tipo(i) = "Equilatero"
  ELSE
    IF ((triangulo(i, 1) /= triangulo(i, 2)) &
         & .AND. (triangulo(i, 2) /= triangulo(i, 3)) .AND. &
         & (triangulo(i, 3) /= triangulo(i, 1))) THEN
      tipo(i) = "Escaleno"
    ELSE
      tipo(i) = "Isosceles"
    END IF
  END IF
END DO
END SUBROUTINE

! Subrotina saida
SUBROUTINE saida(n_triang, triangulo, tipo, nome_triang)

INTEGER                 :: i, n_triang
REAL                    :: triangulo(100,4)
CHARACTER (len=10)      :: nome_triang(100), tipo(100)

! -------------- Modulo: Exibir Relatorio
WRITE (*,*)
WRITE (*,*)

WRITE (*,*) "=========================================================="
WRITE (*,*) "============ RELATORIO DA AREA DO TRIANGULO =============="
WRITE (*,*) "=========================================================="
WRITE (*,*) "===== TRIANGULO ====  BASE  === ALTURA ==    AREA  ======="
DO i = 1, n_triang
  WRITE (*,*) "=                                                        ="
  WRITE (*,50) " = ", i, triangulo(i,1), triangulo(i,2), triangulo(i,4), "  ="
END DO
WRITE (*,*) "=                                                        ="
WRITE (*,*) "=========================================================="

WRITE (*,*)
WRITE (*,*)

WRITE (*,*) "====================================================&
        &==========================="
WRITE (*,*) "=================== RELATORIO DE INFORMACOES DO TRIA&
        &NGULO ====================="
WRITE (*,*) "====================================================&
        &==========================="
WRITE (*,*) "==  NOME  ====    L1    ====    L2    ====    L3&
        &    ====  TIPO         ========"
DO i = 1, n_triang
  WRITE (*,*) "=                                                 &
        &                            ="
  WRITE (*,55) " = ", nome_triang(i), triangulo(i,1), triangulo(i,2), &
        &triangulo(i,3), tipo(i), "     ="
END DO
WRITE (*,*) "=                                                   &
        &                          ="
WRITE (*,*) "====================================================&
        &==========================="

50 FORMAT (A, 8X, I1, 10X, F6.2, 4X, F6.2, 7X, F6.2, 5x, A)
55 FORMAT (A, 2X, A6, 1X, F12.2, 2X, F12.2, 2X, F12.2, 7X, A10, 5X, A)

END SUBROUTINE
