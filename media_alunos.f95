! Esse programa obtem 3 notas de alunos de uma turma e 
! apresenta um relatorio formatado.
PROGRAM Media_dos_Alunos
IMPLICIT NONE

INTEGER :: i, j, Alunos, NEUTRO ! Definindo as variaveis do tipo INTEIRO
CHARACTER (len=50) :: Nome(100) ! Definindo as variaveis do tipo CARACTER
CHARACTER (len=1)  :: sit(100)  ! Definindo as variaveis do tipo CARACTER
REAL :: Nota(100, 4)            ! Definindo as variaveis do tipo REAL

!
! Definindo um valor neutro a variavel Nota
!
Nota = 0


! ===== INICIANDO A APRESENTACAO =========================================
  WRITE (*,*)                   ! Pula uma linha
  WRITE (*,*) "Esse programa obtem as notas dos alunos e apresenta um &
  & relatorio formatado."       ! Escreve na tela texto entre aspas
  WRITE (*,*)                   ! Pula uma linha

                                ! Escreve na tela texto entre aspas
  WRITE (*,*) "Informe a quantidade de alunos na turma: (No Maximo 99)"
  READ  (*,*) Alunos            ! Obtem do teclado um numero e
                                ! armazena na variavel 'Alunos'
  IF (Alunos .GT. 99) THEN      ! Se Alunos for maior que 10, encerra.
    WRITE (*,*)                 ! Pula uma linha
                                ! Escreve na tela texto entre aspas
    WRITE (*,*) "O maximo de alunos que esse programa calcula e 99.&
    & Encerrando"
    STOP                        ! Encerra o programa
  END IF
  WRITE (*,*)                   ! Pula uma linha
                                ! Escreve na tela texto entre aspas
  WRITE (*,*) "Agora entre com os dados solicitados: "
  WRITE (*,*)                   ! Pula uma linha


  ! ===== MODULO: Obter os dados! ========================================
  DO i = 1, Alunos              ! Faz a variavel i variar de 1 ate Alunos
    WRITE (*,50) "Qual o nome completo do aluno ", i, " ?"
    READ  (*,*)  Nome(i)
    WRITE (*,*)                 ! Pula uma linha
    DO j = 1, 3                 ! Inicia a estrutura de repeticao
                                ! Escreve na tela texto entre aspas
      WRITE (*,55) "Informe a nota ", i, " de ", Nome(i)
      READ  (*,*)  Nota(i, j)   ! Obtem do teclado um numero e

                                ! Se Nota(i, j) for maior que 10, encerra.
      IF (Nota(i, j) .GT. 10) THEN
        WRITE (*,*)             ! Pula uma linha
                                ! Escreve na tela texto entre aspas
        WRITE (*,*) "A nota deve ser um numero real de valor maximo igual&
        & a 10. Encerrando."
        STOP                    ! Encerra o programa
      END IF
                                ! armazena no indice (i, j) da variavel
    END DO                      ! Finaliza DO interno (j)
  END DO                        ! Finaliza DO externo (i)
  ! ===== FIM DO MODULO: Obter os dados! =================================


  ! ===== MODULO: Calcular as medias! ====================================
  WRITE (*,*)                   ! Pula uma linha
                                ! Escreve na tela texto entre aspas
  WRITE (*,*) "Gerando o relatorio, por favor aguarde!"
  DO i = 1, Alunos              ! Faz a variavel i variar de 1 ate Alunos
    DO j = 1, 3                 ! Faz a variavel j variar de 1 ate 3
                                ! Soma as 3 notas do aluno
      Nota(i, 4) = Nota(i, j) + Nota(i, 4)
    END DO                      ! Finaliza DO interno (j)
    Nota(i, 4) = Nota(i, 4)/3   ! Calcula a media
  END DO                        ! Finaliza DO externo (i)
                                ! Escreve na tela texto entre aspas
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

  WRITE (*,*)
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
  WRITE (*,*) "=========================================================="

  ! ===== FIM DO MODULO: Calcular situacao do aluno ======================

  WRITE (*,*)                   ! Pula uma linha
  WRITE (*,*)                   ! Pula uma linha


  ! ===== MODULO: Apresentar os dados! ===================================
                                ! Escreve na tela o texto entre aspas
  WRITE (*,*) "=========================================================="
  WRITE (*,*) "================== RELATORIO DA TURMA ===================="
  WRITE (*,*) "=========================================================="
  WRITE (*,*) "==   NOME     == NOTA1 == NOTA2 == NOTA3 == MEDIA == SIT ="
  WRITE (*,*) "=                                                        ="
  DO i = 1, Alunos              ! Faz a variavel i variar de 1 ate Alunos
                                ! Escreve na tela de modo estruturado.
    WRITE (*,60) " = ", Nome(i), (Nota(i, j), j = 1, 4), sit(i), "        ="
  END DO                        ! Finaliza DO (i)
                                ! Escreve na tela o texto entre aspas
  WRITE (*,*) "=                                                        ="
  WRITE (*,*) "=========================================================="

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
  WRITE (*,*)

  ! Exibir o relatorio atualizado
  WRITE (*,*) "=========================================================="
  WRITE (*,*) "================== RELATORIO DA TURMA ===================="
  WRITE (*,*) "=========================================================="
  WRITE (*,*) "==   NOME     == NOTA1 == NOTA2 == NOTA3 == FINAL == SIT ="
  WRITE (*,*) "=                                                        ="
  DO i = 1, Alunos              ! Faz a variavel i variar de 1 ate Alunos
                                ! Escreve na tela de modo estruturado.
    WRITE (*,60) " = ", Nome(i), (Nota(i, j), j = 1, 4), sit(i), "        ="
  END DO                        ! Finaliza DO (i)
                                ! Escreve na tela o texto entre aspas
  WRITE (*,*) "=                                                        ="
  WRITE (*,*) "=========================================================="

  ! ===== FIM DO MODULO: Apresentar os dados!=============================


  WRITE (*,*)                   ! Pula uma linha
  WRITE (*,*)                   ! Pula uma linha


! ====== Sentenca para formatar a saida ==================================
  50 FORMAT (A, I2, A)          ! Formata em modo: Texto; Int 2 casas; Tex
  55 FORMAT (A, I2, A, A)       ! Formata em modo: Texto; Int 2 casas; T;T
                                ! Formata em modo: Texto; Esp. Hor. de 3; 
                                ! 4x de Esp. Hor. de 3 e Real 6 ca. 2 Dec;
                                ! Texto;
  60 FORMAT (A, 3XA8, 4(3X, F6.2), 2x, A, A)


! ===== Finalizando o programa ===========================================
!
END PROGRAM Media_dos_Alunos

