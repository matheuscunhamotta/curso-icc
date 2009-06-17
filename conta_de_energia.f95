PROGRAM Conta_de_Energia
IMPLICIT NONE

CHARACTER (len=10)      :: nome(100)
INTEGER                 :: i, residencias
REAL                    :: kwh(100), leitura(100,5), conta(100,2)

WRITE (*,*) "Informe a quantidade de residencias: (NO MAXIMO 100)"
READ  (*,*) residencias
WRITE (*,*)

DO i = 1, residencias
  ! Obtemos os dados dos usuarios e armazenamos nas variaveis
  WRITE (*,1) "Qual o nome do proprietario da residencia ", i, " :"
  READ  (*,*) nome(i)
  WRITE (*,*)
  WRITE (*,*)
  WRITE (*,2) "Informe a leitura inicial da residencia de ", nome(i), " :"
  READ  (*,*) leitura(i, 1)
  WRITE (*,2) "Informe a leitura final da residencia de ", nome(i), " :"
  READ  (*,*) leitura(i, 2)
  WRITE (*,*)
  WRITE (*,*) "________________________________________________________"
  WRITE (*,*)
END DO

DO i = 1, residencias
  ! Calcula o consumo
  leitura(i, 3) = (leitura(i, 2) - leitura(i, 1))

  ! Verifica o consumo e aplica uma taxa correspondente ao nivel de consumo
  IF (leitura(i, 3) <= 10) THEN
    kwh(i) = 0.10
  ELSE IF ((leitura(i, 3) > 10) .AND. (leitura(i, 3) <= 30)) THEN
    kwh(i) = 0.20
  ELSE
    IF ((leitura(i, 3) > 30) .AND. (leitura(i, 3) <= 50)) THEN
      kwh(i) = 0.30
    ELSE
      kwh(i) = 0.60
    END IF
  END IF

  ! Verifica o consumo e aplica uma taxa percentual correspondente ao nivel de
  ! consumo
  IF (leitura(i, 3) <= 30) THEN
    leitura(i, 5) = 0.10
  ELSE IF ((leitura(i, 3) > 30) .AND. (leitura(i, 3) <= 50)) THEN
    leitura(i, 5) = 0.20
  ELSE
    IF ((leitura(i, 3) > 50) .AND. (leitura(i,3) <= 100)) THEN
      leitura(i, 5) = 0.30
    ELSE
      leitura(i, 5) = 0.50
    END IF
  END IF

  ! Multiplica o valor do consumo pela taxa cobrada por kwh (Kilowats/Hora)
  leitura(i, 4) = leitura(i, 3) * kwh(i)

  ! Calcula a quantia de acordo com a porcentagem
  conta(i, 1) = leitura(i, 4) * leitura(i, 5)

  ! Calcula o valor total da conta
  conta(i, 2) = leitura(i, 4) + conta(i, 1)
END DO

WRITE (*,*)
WRITE (*,*)

! Apresenta o relatorio
WRITE (*,*) "====================================================&
        &==========================="
WRITE (*,*) "====================== RELATORIO DA CONTA DE ENERGIA&
        & =========================="
WRITE (*,*) "====================================================&
        &==========================="
WRITE (*,*) "= RESIDENCIA = L.INICIAL = L.FINAL = CONSUMO = VWH =&
        & V.CONSUMO = TAXA = CONTA ="
DO i = 1, residencias
  ! INDICE DAS VARIAVEIS - Cada linha corresponde a uma residencia!
  ! -- e cada coluna corresponde a seguinte estrutura:
  !
  !!!!!  leitura(n, 1) = L.Inicial;
  !!!!!  leitura(n, 2) = L.Final;
  !!!!!  leitura(n, 3) = Consumo;
  !!!!!  leitura(n, 4) = Valor Consumo;
  !!!!!  leitura(n, 5) = Taxa  Percentual;
  !!!!!  conta(n, 1)   = Valor Percentual;
  !!!!!  conta(n, 2)   = Valor Total da conta de energia;
  WRITE (*,*) "=                                                 &
        &                            ="
  WRITE (*,50) " = ", nome(i), leitura(i, 1), leitura(i, 2), leitura(i, 3), &
    &kwh(i), leitura(i, 4), conta(i, 1), conta(i, 2), " ="
END DO
WRITE (*,*) "====================================================&
        &==========================="

1 FORMAT (A, I3, A)
2 FORMAT (A, A8, A)
50 FORMAT (A, A10, 3X, F6.2, 6X, F6.2, 3X, F6.2, 3X, F6.2, 1X, F6.2, &
        &5X, F6.2, 2X, F6.2, A)

END PROGRAM
