      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      ************************************************************
      *    �ǡ��������å���ǧ����        COPY (CPDTCHKKOMOKU.INC)*
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  05.01.01    ORCAMO       22/10/26  ���֥ǡ��������å��б�
      ************************************************************
       01  DTCHKKOMOKU-AREA.
           03  DTCHKKOMOKU-VAL.
               05                          PIC X(02)   VALUE
               "01".
               05                          PIC X(40)   VALUE
               "�ݸ����ֹ�".
               05                          PIC X(50)   VALUE
               "InsuranceProvider_Number".
               05                          PIC X(02)   VALUE
               "02".
               05                          PIC X(40)   VALUE
               "���ݸ��Ե��桦�ֹ�".
               05                          PIC X(50)   VALUE
               "HealthInsuredPerson_Symbol_And_Number".
               05                          PIC X(02)   VALUE
               "03".
               05                          PIC X(40)   VALUE
               "������ô���ֹ桦������ֹ�".
               05                          PIC X(50)   VALUE
               "PublicInsurance".
               05                          PIC X(02)   VALUE
               "04".
               05                          PIC X(40)   VALUE
               "�ݸ�Ŭ��ǯ��".
               05                          PIC X(50)   VALUE
               "Age".
               05                          PIC X(02)   VALUE
               "05".
               05                          PIC X(40)   VALUE
               "������̾".
               05                          PIC X(50)   VALUE
               "Disease".
               05                          PIC X(02)   VALUE
               "06".
               05                          PIC X(40)   VALUE
               "���ų�����".
               05                          PIC X(50)   VALUE
               "Disease_StartDate".
               05                          PIC X(02)   VALUE
               "07".
               05                          PIC X(40)   VALUE
               "������̾��������̾".
               05                          PIC X(50)   VALUE
               "Disease_Suspected_And_Disease_Acute".
               05                          PIC X(02)   VALUE
               "08".
               05                          PIC X(40)   VALUE
               "���ޤ���̾".
               05                          PIC X(50)   VALUE
               "Medicine_With_Disease".
               05                          PIC X(02)   VALUE
               "09".
               05                          PIC X(40)   VALUE
               "���Ź԰٤���̾".
               05                          PIC X(50)   VALUE
               "Medication_With_Disease".
               05                          PIC X(02)   VALUE
               "10".
               05                          PIC X(40)   VALUE
               "���ޤ�ʻ�Ѷش�".
               05                          PIC X(50)   VALUE
               "Medicine_Interact".
               05                          PIC X(02)   VALUE
               "11".
               05                          PIC X(40)   VALUE
               "���Ź԰٤�ʻ�ѻ���".
               05                          PIC X(50)   VALUE
               "Medication_Combination".
               05                          PIC X(02)   VALUE
               "12".
               05                          PIC X(40)   VALUE
               "������".
               05                          PIC X(50)   VALUE
               "Actual_Days".
               05                          PIC X(02)   VALUE
               "13".
               05                          PIC X(40)   VALUE
               "���Ի�̾".
               05                          PIC X(50)   VALUE
               "WholeName".
               05                          PIC X(02)   VALUE
               "14".
               05                          PIC X(40)   VALUE
               "�ݸ��ȹ礻ͭ������".
               05                          PIC X(50)   VALUE
               "Insurance_ExpiredDate".
               05                          PIC X(02)   VALUE
               "15".
               05                          PIC X(40)   VALUE
               "��Ϳ�ش����ޤ���̾".
               05                          PIC X(50)   VALUE
               "Medicine_Disease_Interact".
               05                          PIC X(02)   VALUE
               "16".
               05                          PIC X(40)   VALUE
               "���Ź԰٤ɤ����λ���ϳ��".
               05                          PIC X(50)   VALUE
               "Medication_Combination_Leak".
               05                          PIC X(02)   VALUE
               "17".
               05                          PIC X(40)   VALUE
               "��̾������".
               05                          PIC X(50)   VALUE
               "Disease_With_Medicine".
               05                          PIC X(02)   VALUE
               "18".
               05                          PIC X(40)   VALUE
               "��̾�ȿ��Ź԰�".
               05                          PIC X(50)   VALUE
               "Disease_With_Medication".
               05                          PIC X(02)   VALUE
               "19".
               05                          PIC X(40)   VALUE
               "���ޤη��²��������Ϳ��".
               05                          PIC X(50)   VALUE
               "Medicine_Dosage".
               05                          PIC X(02)   VALUE
               "20".
               05                          PIC X(40)   VALUE
               "���Ź԰ٻ�����".
               05                          PIC X(50)   VALUE
               "Medication_Count".
               05                          PIC X(02)   VALUE
               "21".
               05                          PIC X(40)   VALUE
               "���򼰥�����".
               05                          PIC X(50)   VALUE
               "Selective_Comment".
               05                          PIC X(02)   VALUE
               "22".
               05                          PIC X(40)   VALUE
               "���ݸ��Ի���".
               05                          PIC X(50)   VALUE
               "HealthInsuredPerson_BranchNumber".
           03  DTCHKKOMOKU-R   REDEFINES   DTCHKKOMOKU-VAL.
               05  DTCHKKOMOKU-CHKKOMOKU-OCC           OCCURS  22.
                   07  DTCHKKOMOKU-CHKNUM      PIC X(02).
                   07  DTCHKKOMOKU-CHKKOMOKU   PIC X(40).
                   07  DTCHKKOMOKU-CHKKOMOKUID PIC X(50).
           03  DTCHKKOMOKU-MAX             PIC 9(03)   VALUE   22.
      *
       01  DTCHKHOKEN-AREA.
           03  DTCHKHOKEN-VAL.
               05  DTCHKHOKEN-SHAHO            PIC X(40)   VALUE
               "����".
               05  DTCHKHOKEN-SHAHOID          PIC X(50)   VALUE
               "Employees_Health_Insurance".
               05  DTCHKHOKEN-KOKUHO           PIC X(40)   VALUE
               "����".
               05  DTCHKHOKEN-KOKUHOID         PIC X(50)   VALUE
               "National_Health_Insurance".
               05  DTCHKHOKEN-KOKI             PIC X(40)   VALUE
               "��������".
               05  DTCHKHOKEN-KOKIID           PIC X(50)   VALUE
               "Late_Elderly_Health_Insurance".
               05  DTCHKHOKEN-RSIJBI           PIC X(40)   VALUE
               "ϫ�ҡ������ա��軰�Թ԰�".
               05  DTCHKHOKEN-RSIJBIID         PIC X(50)   VALUE
               "Accident_Insurance".
               05  DTCHKHOKEN-KOGAI            PIC X(40)   VALUE
               "����".
               05  DTCHKHOKEN-KOGAIID          PIC X(50)   VALUE
               "Pollution_Insurance".
               05  DTCHKHOKEN-JIHI             PIC X(40)   VALUE
               "����".
               05  DTCHKHOKEN-JIHIID           PIC X(50)   VALUE
               "Own_Expense".
           03  DTCHKHOKEN-R    REDEFINES   DTCHKHOKEN-VAL.
               05  DTCHKHOKEN-CHKHOKEN-OCC             OCCURS  6.
                   07  DTCHKHOKEN-CHKHOKEN     PIC X(40).
                   07  DTCHKHOKEN-CHKHOKENID   PIC X(50).
           03  DTCHKHOKEN-MAX              PIC 9(03)   VALUE   6.
