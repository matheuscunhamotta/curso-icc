PROGRAM Producoes
      IMPLICIT NONE

      REAL              ::      prod_ind(100,3) ! 3 Empresas, 3 Colunas
      REAL              ::      soma_ind(3), med_ind(3)
      INTEGER           ::      i, numdias, dia(100)

      WRITE (*,*) "Digite o numero de dias que deseja analisar:"
      READ  (*,*) numdias
      WRITE (*,*)

      CALL dados(numdias, prod_ind, soma_ind, med_ind, dia)

      ! Exibindo o relatorio na tela
      WRITE (*,*)
      DO i = 1, 3
        WRITE (*,*) "------------ MEDIA DE PRODUCAO ------------"
        WRITE (*,*)
        WRITE (*,1) "A media da industria ", i, " e:", med_ind(i)
        WRITE (*,*) "-------------------------------------------"
        WRITE (*,*)
      END DO
      1 FORMAT (2X, A, I0, A, F14.4)
END PROGRAM

SUBROUTINE dados(numdias, prod_ind, soma_ind, med_ind, dia)
      INTEGER           ::      i, k, numdias, dia(100)
      REAL              ::      prod_ind(100, 3), soma_ind(3), med_ind(3)
      CHARACTER*100     ::      lm1, lm2

      ! Lendo os dados dos arquivos
      DO k = 1, 3
        DO i = 1, numdias
          WRITE (lm1,'(A,I0,A)') 'entrada',k,'.txt'
          OPEN  (k*10,file=lm1)
          READ  (k*10,5) dia(i), prod_ind(i,k)
        END DO
        CLOSE (k*10)
      END DO
      5 FORMAT (I2, F11.3)

      ! Calculando os dados
      soma_ind(1:3) = 0
      DO k = 1, 3
        DO i = 1, numdias
          soma_ind(k) = soma_ind(k) + prod_ind(i,k)
        END DO
      END DO
      DO k = 1, 3
        med_ind(k) = soma_ind(k) / numdias
      END DO

      ! Gravando os dados nos arquivos
      DO i = 1, 3
        WRITE (lm2, '(A,I0,A)') 'saida',i,'.txt'
        OPEN  (i*100,file=lm2)
        WRITE (i*100,*) "-------- INDUSTRIA ", i, " --------"
        WRITE (i*100,*)
        WRITE (i*100,*) "Referente a ", numdias, " dias uteis:"
        WRITE (i*100,*)
        WRITE (i*100,6) "A media de producao e: ", med_ind(i)
        CLOSE (i*100)
      END DO
      6 FORMAT (A, 4X, F11.3)
END SUBROUTINE
