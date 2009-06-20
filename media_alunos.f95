PROGRAM Media_dos_Alunos
IMPLICIT NONE

INTEGER :: i, j, Alunos, NEUTRO
CHARACTER (len=50) :: Nome(100)
CHARACTER (len=1)  :: sit(100)
REAL :: Nota(100, 4)

CALL entrada(Alunos, Nome, Nota)
CALL calcula(Alunos, sit, Nota)
CALL saida(Alunos, Nome, sit, Nota)

END PROGRAM Media_dos_Alunos

! Subrotina entrada
SUBROUTINE entrada(Alunos, Nome, Nota)

INTEGER :: i, j, Alunos
CHARACTER (len=50) :: Nome(100)
REAL :: Nota(100, 4)

! Definindo um valor neutro a variavel Nota
Nota = 0

! ===== INICIANDO A APRESENTACAO =========================================
  WRITE (*,*)
  WRITE (*,*) "Esse programa obtem as notas dos alunos e apresenta um &
  & relatorio formatado."
  WRITE (*,*)

  WRITE (*,*) "Informe a quantidade de alunos na turma: (No Maximo 99)"
  READ  (*,*) Alunos

  IF (Alunos .GT. 99) THEN
    WRITE (*,*)
    WRITE (*,*) "O maximo de alunos que esse programa calcula e 99.&
    & Encerrando"
    STOP
  END IF
  WRITE (*,*)
  WRITE (*,*) "Agora entre com os dados solicitados: "

  ! ===== MODULO: Obter os dados! ========================================
  DO i = 1, Alunos
    WRITE (*,50) "Qual o nome completo do aluno ", i, " ?"
    READ  (*,*)  Nome(i)
    WRITE (*,*)
    DO j = 1, 3
      WRITE (*,55) "Informe a nota ", j, " de ", Nome(i)
      READ  (*,*)  Nota(i, j)
      IF (Nota(i, j) .GT. 10) THEN
        WRITE (*,*)
        WRITE (*,*) "A nota deve ser um numero real de valor maximo igual&
        & a 10. Encerrando."
        STOP
      END IF
    END DO
  END DO
  ! ===== FIM DO MODULO: Obter os dados! =================================
 WRITE (*,*)

50 FORMAT (A, I2, A)
55 FORMAT (A, I2, A, A)
END SUBROUTINE

! Subrotina calcula
SUBROUTINE calcula(Alunos, sit, Nota)

INTEGER :: i, j, Alunos
CHARACTER (len=1)  :: sit(100)
REAL :: Nota(100, 4)

  ! ===== MODULO: Calcular as medias! ====================================
  WRITE (*,*)
  WRITE (*,*) "Gerando o relatorio, por favor aguarde!"
  DO i = 1, Alunos
    DO j = 1, 3
      Nota(i, 4) = Nota(i, j) + Nota(i, 4)
    END DO
    Nota(i, 4) = Nota(i, 4)/3
  END DO
  WRITE (*,*) "Operacao concluida, exibindo agora os resultados:"
  ! ===== FIM DO MODULO: Calcular as medias! =============================

  ! ===== MODULO: Calcular situacao do aluno =============================
  DO i = 1, Alunos
    IF (Nota(i, 4) >= 7) THEN
      sit(i) = "A"
    ELSE
      IF (Nota(i, 4)  < 4) THEN
        sit(i) = "R"
      ELSE
        sit(i) = "F"
      END IF
    END IF
  END DO
END SUBROUTINE

! Subrotina saida
SUBROUTINE saida(Alunos, Nome, sit, Nota)

INTEGER :: i, j, Alunos, tmp = 0
CHARACTER (len=50) :: Nome(100)
CHARACTER (len=1)  :: sit(100)
REAL :: Nota(100, 4)

DO i = 1, Alunos
  IF (sit(i) == "F") THEN
    tmp = tmp + 1
  END IF
END DO

IF (tmp > 0) THEN
  WRITE (*,*)
  WRITE (*,*) "=========================================================="
  WRITE (*,*) "========= RELACAO DOS ALUNOS QUE IRAM PRA FINAL =========="
  WRITE (*,*) "=========================================================="
  WRITE (*,*) "=== NOME ==== SITUACAO ==================================="
  DO i = 1, Alunos
    IF (sit(i) == "F") THEN
      WRITE (*,*) Nome(i), " Esta na final!!!"
    END IF
  END DO
  WRITE (*,*) "    Total de alunos na final: ", tmp
  WRITE (*,*) "=========================================================="
END IF
  ! ===== FIM DO MODULO: Calcular situacao do aluno ======================

  WRITE (*,*)

  ! ===== MODULO: Apresentar os dados! ===================================
  WRITE (*,*) "=========================================================="
  WRITE (*,*) "================== RELATORIO DA TURMA ===================="
  WRITE (*,*) "=========================================================="
  WRITE (*,*) "==   NOME     == NOTA1 == NOTA2 == NOTA3 == MEDIA == SIT ="
  WRITE (*,*) "=                                                        ="
  DO i = 1, Alunos
    WRITE (*,60) " = ", Nome(i), (Nota(i, j), j = 1, 4), sit(i), "        ="
  END DO
  WRITE (*,*) "=                                                        ="
  WRITE (*,*) "=========================================================="

IF (tmp > 0) THEN
  ! Calcular a media atualizada dos alunos que fizeram prova final.
  WRITE (*,*)
  WRITE (*,*)
  DO i = 1, Alunos
    IF (sit(i) == "F") THEN
      WRITE (*,*) "Entre com a nota da prova FINAL de ", Nome(i)
      READ  (*,*) Nota(i, 4)
      IF (Nota(i, 4) >= 7) THEN
        sit(i) = "A"
      ELSE
        sit(i) = "R"
      END IF
    END IF
  END DO

  WRITE (*,*)

  ! Exibir o relatorio atualizado
  WRITE (*,*) "=========================================================="
  WRITE (*,*) "================== RELATORIO DA TURMA ===================="
  WRITE (*,*) "=========================================================="
  WRITE (*,*) "==   NOME     == NOTA1 == NOTA2 == NOTA3 == FINAL == SIT ="
  WRITE (*,*) "=                                                        ="
  DO i = 1, Alunos
    WRITE (*,60) " = ", Nome(i), (Nota(i, j), j = 1, 4), sit(i), "        ="
  END DO
  WRITE (*,*) "=                                                        ="
  WRITE (*,*) "=========================================================="
END IF
  ! ===== FIM DO MODULO: Apresentar os dados!=============================

  WRITE (*,*)
  WRITE (*,*)

! ====== Sentenca para formatar a saida ==================================
60 FORMAT (A, 3XA8, 4(3X, F6.2), 2x, A, A)
END SUBROUTINE
