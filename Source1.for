C     PROGRAM PRO RAZENI POLE
C     AUTOR: TOMAS CHABADA P4

C     POMOCNE METODY
      MODULE METODY
      CONTAINS
      
C     METODA PRO NAHODNE NAPLNENI DVOU POLI CISLY
      SUBROUTINE NAHODNA_CISLA(POLE1, POLE2)
          IMPLICIT NONE
          INTEGER, DIMENSION(:), INTENT(INOUT) :: POLE1, POLE2
          INTEGER :: DELKA_POLE, I
          DOUBLE PRECISION :: NAHODNE_CISLO
          
          DELKA_POLE = SIZE(POLE1)
      
C         CYKLUS PRES VSECHNY PRVKY POLE   
          DO I=1,DELKA_POLE
              CALL RANDOM_NUMBER(NAHODNE_CISLO)
              POLE1(I) = NAHODNE_CISLO * HUGE(DELKA_POLE)
              POLE2(I) = NAHODNE_CISLO * HUGE(DELKA_POLE)
          END DO         
      END SUBROUTINE NAHODNA_CISLA    
      
C     METODA PRO POROVNAVANI STEJNYCH POLI
      SUBROUTINE POROVNEJ_POLE(POLE1, POLE2, ERROR)
          IMPLICIT NONE
          INTEGER, DIMENSION(:), INTENT(INOUT) :: POLE1, POLE2
          INTEGER, INTENT(INOUT) :: ERROR
          INTEGER :: I
          
          ERROR = 0
          DO I = 1, SIZE(POLE1)
              IF (POLE1(I) /= POLE2(I)) THEN
                  ERROR = I
                  EXIT
              END IF
          END DO
      END SUBROUTINE POROVNEJ_POLE
      
C     METODA PRO PODMINENE PREHOZENI
      ELEMENTAL SUBROUTINE PROHOD_PODMINENE(PRVEK1, PRVEK2)
          INTEGER :: POMOCNA
          INTEGER, INTENT(INOUT) :: PRVEK1, PRVEK2
          IF (PRVEK1 > PRVEK2) THEN
              POMOCNA = PRVEK1
              PRVEK1 = PRVEK2
              PRVEK2 = POMOCNA
          END IF
      END SUBROUTINE PROHOD_PODMINENE
      END MODULE METODY
     
C     HLAVNI PROGRAM
      PROGRAM MAIN
          USE METODY
          IMPLICIT NONE
          
          INTEGER :: POCET_PRVKU = 500
          INTEGER, PARAMETER :: POCET_VLAKEN = 8
          INTEGER, DIMENSION(:), ALLOCATABLE :: POLE1, POLE2
          INTEGER :: I,J,K,POMOCNA,ERROR
          
C         ALOKACE POLI PRO          
          INTEGER, DIMENSION(POCET_VLAKEN) :: UKAZATELE          
          ALLOCATE(POLE1(POCET_PRVKU))       
          ALLOCATE(POLE2(POCET_PRVKU))
          
C         INICIALIZACE UKAZATELU (ROVNOMERNE ROZLOZENI)                 
          DO I = 1,POCET_VLAKEN
              UKAZATELE(I) = (I-1) * (POCET_PRVKU/POCET_VLAKEN) +1
          END DO
          
C         NAPLNENI POLE NAHODNYMI CISLY         
          CALL NAHODNA_CISLA(POLE1, POLE2)
          
C         SETRIZENI POLE JEDNOVLAKNOVE
          PRINT*, "TRIDENI - JEDNOVLAKNOVE"
          DO I=1,POCET_PRVKU
              DO J=1,POCET_PRVKU-1
                  IF (POLE1(J) > POLE1(J+1)) THEN
                      POMOCNA = POLE1(J)
                      POLE1(J) = POLE1(J+1)
                      POLE1(J+1) = POMOCNA
                  END IF
              END DO
          END DO
         PRINT*, "SETRIZENO - JEDNOVLAKNOVE"
          
C         SETRIZENI POLE VICEVLAKNOVE
       DO CONCURRENT(I = 1:POCET_VLAKEN)
          !DIR$ NOPARALLEL
          DO J = 1, POCET_PRVKU
          !DIR$ NOPARALLEL
          DO K = 1, POCET_PRVKU / POCET_VLAKEN
              
         CALL PROHOD_PODMINENE(POLE2(UKAZATELE(I)), 
     &    POLE2(UKAZATELE(I)+1))
              UKAZATELE(I) = UKAZATELE(I) + 1
              IF (UKAZATELE(I) == POCET_PRVKU) THEN
                  UKAZATELE(I) = 1
              END IF
          END DO
          END DO
      END DO
              
C         POROVNANI POLI
          CALL POROVNEJ_POLE(POLE1, POLE2, ERROR)
          
C         VYPIS DAT DO SOUBORU
          OPEN(10, FILE="VYSTUP.TXT", STATUS="REPLACE")
          WRITE(10, *) ERROR         
          WRITE(10, "(A)") "PRVNI POLE"
          WRITE(10, *) POLE1
          WRITE(10, "(A)") "DRUHE POLE"
          WRITE(10, *) POLE2
          
          CLOSE(10)
          PAUSE
      END PROGRAM MAIN