Public Class Funzione

    'Conterrà il tipo della funzione in oggetto, i valori possibili si trovano definiti come costanti in fondo
    'alla classe, inizialmente è invalida
    Private tipo As Integer = INVALIDA

    'Conterrà eventuali argomenti della funzione (che saranno a loro volta delle funzioni)
    Private argomenti As Funzione()

    'Conterrà l'espressione in forma testuale
    Private _espressione As String

    'Nel caso di una funzione costante conterrà il valore della costante stessa
    Private valore As Double

    'Unico costruttore, prende come unico argomento obbligaotorio una string
    Public Sub New(ByVal espressione As String)

        Debug.WriteLine("Costruzione di una nuova funzione con espressione: " + espressione)
        'Ottimizzazione della stringa espressione
        RimuoviSpazi(espressione)

        If Not PulisciParentesi(espressione) Then
            'L'esecuzione di PulisciParentesi(String) restituisce valore falso se è stata riscontrato un
            'errore relativo al conto delle parentesi, la funzione rimane di tipo invalido e la costruzione
            'dell'oggetto si interrompe 
            Return
        End If
        _espressione = espressione

        'Questa procedura identifica e costruisce la funzione e i suoi argomenti:
        Definisci(espressione)
        calcolaCostante()
        If tipo = ESPONENZIALE Then
            filtraPotenze()
        End If
        Debug.WriteLine("tipo: " + CStr(tipo))

        Debug.WriteLine("")
    End Sub

    'Per calcolare il valore della funzione, in modo ricorsivo
    'Se la funzione è x o è costante allora risponde rispettivamente con il valore inserito o con il valore 
    'della costante
    'Negli altri casi la funzione viene calcolata usando i propri argomenti. Essendo gli argomenti a loro
    'volta delle funzioni è necessario calcolarne il valore
    'Una funzione invalida resituisce il valore speciale NaN (Non un numero)
    Public Function Calcola(ByVal x As Double) As Double
        Select Case tipo
            Case COSTANTE

                Return valore
            Case Funzione.X
                Return x
            Case SOMMA
                Return argomenti(0).Calcola(x) + argomenti(1).Calcola(x)
            Case DIFFERENZA
                Return argomenti(0).Calcola(x) - argomenti(1).Calcola(x)
            Case PRODOTTO
                Return argomenti(0).Calcola(x) * argomenti(1).Calcola(x)
            Case QUOZIENTE
                Return argomenti(0).Calcola(x) / argomenti(1).Calcola(x)
            Case POTENZA
                Return argomenti(0).Calcola(x) ^ argomenti(1).Calcola(x)
            Case ESPONENZIALE
                Return argomenti(0).Calcola(x) ^ argomenti(1).Calcola(x)
            Case LOGARITMO
                Return Math.Log(argomenti(0).Calcola(x), argomenti(1).Calcola(x))
            Case VALORE_ASSOLUTO
                Return Math.Abs(argomenti(0).Calcola(x))
            Case SENO
                Return Math.Sin(argomenti(0).Calcola(x))
            Case COSENO
                Return Math.Cos(argomenti(0).Calcola(x))
            Case TANGENTE
                Return Math.Tan(argomenti(0).Calcola(x))
            Case INVALIDA
                Return Double.NaN
        End Select
        'Questa ultima riga in teoria non si esegue MAI è conveniente averla per evitare la segnalazione
        'da parte di Visual Studio
        Return Double.NaN
    End Function

    'Per ottenere la stringa che definisce la funzione
    Public ReadOnly Property Espressione() As String
        Get
            Return _espressione
        End Get
    End Property

    'Proprietà di sola lettura della funzione 
    'risponde con vero se la funzione è calcolabile
    Public ReadOnly Property Valida() As Boolean
        Get

            If tipo = INVALIDA Then
                'Se il tipo è INVALIDA non guardo oltre e rispondo con falso
                Return False

            ElseIf tipo = COSTANTE Or tipo = X Then
                'Se la funzione è costante o è uguale alla variabile risponde sempre con vero
                Return True

            Else
                'Altrimenti la funzione è valida solo se sono validi tutti i suoi argomenti
                'guardo una funzione alla volta e appena ne trovo una invalida rispondo con falso
                For Each arg As Funzione In argomenti
                    If Not arg.Valida Then
                        Return False
                    End If
                Next
                'se non ci sono argomenti invalidi rispondo con vero
                Return True

            End If

        End Get
    End Property

    Public ReadOnly Property Derivabile() As Boolean
        Get
            If Derivata.Valida Then
                Return True
            Else
                Return False
            End If
        End Get
    End Property

    'Proprietà di sola lettura
    'restituisce la funzione derivata
    Public ReadOnly Property Derivata() As Funzione
        Get
            Debug.WriteLine("Definendo la derivata di "+ _espressione)
            Select Case tipo
                Case COSTANTE
                    Return New Funzione("0")
                Case Funzione.X
                    Return New Funzione("1")
                Case SOMMA
                    Return argomenti(0).Derivata + argomenti(1).Derivata
                Case DIFFERENZA
                    Return argomenti(0).Derivata - argomenti(1).Derivata
                Case PRODOTTO
                    Return argomenti(0).Derivata * argomenti(1) + argomenti(1).Derivata * argomenti(0)
                Case QUOZIENTE
                    Return (argomenti(0).Derivata * argomenti(1) - argomenti(1).Derivata * argomenti(0)) _
                        / (argomenti(1) ^ New Funzione("2"))
                Case POTENZA
                    Return argomenti(1) * argomenti(0) ^ (argomenti(1) - New Funzione("1")) * argomenti(0).Derivata
                Case ESPONENZIALE
                    Return Me * (argomenti(1).Derivata * New Funzione("ln(" + argomenti(0).Espressione + ")") _
                        + argomenti(1) * argomenti(0).Derivata / argomenti(0))
                Case LOGARITMO
                    'Questa è stata tosta, ho dovuto dedurre una formula risolutiva per il caso più generico
                    'possibile: il logaritmo in base g(x) di f(x). Per renderlo affrontabile, con il cambio
                    'di base lo si può trasformare nel rapporto tra i logaritmi in base e di f(x) e g(x).
                    'Applicando la regola della derivata di un quoziente e poi quelle della derivata della 
                    'funzione composta e del logaritmo naturale sono arrivato a un risultato che ho verificato
                    'tramite wolframalpha.com (LINK:http://goo.gl/pUwjhy) e ulteriormente semplificato su 
                    'carta e applicato di seguito
                    Return (argomenti(0).Derivata / argomenti(0) - Me * argomenti(1).Derivata / argomenti(1)) _
                        / New Funzione("ln(" + argomenti(1).Espressione + ")")

                Case VALORE_ASSOLUTO
                    Return New Funzione("")
                Case SENO
                    Return New Funzione("cos(" + argomenti(0).Espressione + ")") * argomenti(0).Derivata
                Case COSENO
                    Return New Funzione("-sin(" + argomenti(0).Espressione + ")") * argomenti(0).Derivata
                Case TANGENTE
                    Return New Funzione(1) / New Funzione("cos(" + argomenti(0).Espressione + ")") * argomenti(0).Derivata
                Case INVALIDA
                    Return New Funzione("")
            End Select
            Return New Funzione("")
        End Get
    End Property

    'I seguenti Operator permettono di semplificare la creazione di somme, differenze, ecc di funzioni
    Public Shared Operator +(ByVal f1 As Funzione,
                             ByVal f2 As Funzione) As Funzione
        Return New Funzione(f1.Espressione + "+" + f2.Espressione)
    End Operator

    Public Shared Operator -(ByVal f1 As Funzione,
                         ByVal f2 As Funzione) As Funzione
        Return New Funzione(f1.Espressione + "-" + f2.Espressione)
    End Operator

    Public Shared Operator *(ByVal f1 As Funzione,
                         ByVal f2 As Funzione) As Funzione
        Return New Funzione("(" + f1.Espressione + ")*(" + f2.Espressione + ")")
    End Operator

    Public Shared Operator /(ByVal f1 As Funzione,
                         ByVal f2 As Funzione) As Funzione
        Return New Funzione("(" + f1.Espressione + ")/(" + f2.Espressione + ")")
    End Operator

    Public Shared Operator ^(ByVal f1 As Funzione,
                         ByVal f2 As Funzione) As Funzione
        Return New Funzione("(" + f1.Espressione + ")^(" + f2.Espressione + ")")
    End Operator

    'Questa procedura identifica e costruisce la funzione e i suoi argomenti
    Private Sub Definisci(ByRef espressione As String)
        ReDim argomenti(-1)
        'Se l'espressione è una stringa vuota la funzione è invalida e si termina la costruzione
        If espressione = "" Then
            Return
        End If

        'Se la funzione è preceduta da segno - o + viene anteposta ad essa lo 0 in modo da renderla più facile
        'da interpretare, per esempio -4 diventa 0-4 
        If espressione(0) = "-" Or espressione(0) = "+" Then
            espressione = espressione.Insert(0, "0")
        End If

        'Se la funzione inzia con un operatore (esclusi + o -) essa è invalida
        If espressione(0) = "/" Or espressione(0) = "*" Or espressione(0) = "^" Then
            Return
        End If

        'Se la funzione finsice con un operatore essa è invalida 
        If espressione(espressione.Length - 1) = "/" Or espressione(espressione.Length - 1) = "*" _
        Or espressione(espressione.Length - 1) = "^" Or espressione(espressione.Length - 1) = "-" _
        Or espressione(espressione.Length - 1) = "+" Then
            Return
        End If

        If Not contieneSimboli(espressione) Then
            'Se la funzione non contiene simboli non avrà sicuramente argomenti
            'possiamo quindi inizializzare l'array degli argomenti ad un array di lunghezza zero nel modo
            'seguente
            ReDim argomenti(-1)

            'Le tre condizioni seguenti sono abbastanza auto esplicative...

            If espressione = "x" Then
                Debug.WriteLine("Riscontrata variabile")
                tipo = X
                Return
            End If

            If espressione = "pi" Then
                Debug.WriteLine("Trovata costante: pi greco")

                tipo = COSTANTE
                valore = Math.PI
                Return
            End If

            If espressione = "e" Then
                Debug.WriteLine("Trovata costante: numero di Nepero")

                tipo = COSTANTE
                valore = Math.E
                Return
            End If

            'Di seguito viene interpretata una costante espressa come numero decimale
            'La funzione TryParse trasforma la stringa del primo argomento in un numero reale e lo assegna
            'alla variabile indicata dal secondo parametro, il ritorno è di tipo boolean, vero nel caso di
            'riuscita della trasformazione, falso altrimenti
            If Double.TryParse(espressione, valore) Then
                Debug.WriteLine("Trovata costante: " + CStr(valore))
                tipo = COSTANTE
                Return
            Else
                Return
            End If
        End If

        'Ora cerchiamo operatori binari (+ - * / ^)

        'in questa variabile consereremo l'indice dell'operatore trovato, assegnamo un valore iniziale di 0
        'se il valore raggiunge la fine della stringa significa che non sono presenti operatori, non c'è
        'ambiguità perchè l'ultimo carattere a questo punto (dopo le elaborazioni precedenti) non potrà essere
        'un operatore
        Dim indiceOperatore As Integer = 0

        'Eseguiamo un ciclo che passa ogni indice valido per i caratteri dell'espressione
        For i = 0 To espressione.Length - 1

            'Poichè stiamo cercando gli operatori più superficiali, cioè quelli che vengono eseguiti per
            'ultimi se stessimo calcolando la funzione, saltiamo tutti i pezzi compresi tra parentesi
            If espressione(i) = "(" Then
                Dim aperte As Integer = 1
                While Not aperte = 0 And i < espressione.Length - 1
                    i = i + 1
                    If espressione(i) = "(" Then
                        aperte = aperte + 1
                    End If
                    If espressione(i) = ")" Then
                        aperte = aperte - 1
                    End If
                End While
            End If

            'se il carattere osservato ha priorità di operatore inferiore del precedente operatore inferiore
            'trovato, assegnamo ad indiceOperatore l'indice osservato in questo momento
            If priorita(espressione(i)) >= priorita(espressione(indiceOperatore)) Then
                indiceOperatore = i
            End If
        Next

        'se l'indice operatore non ha raggiunto la fine della stringa, singifica che abbiamo trovato un 
        'operatore binario
        If Not indiceOperatore = espressione.Length - 1 Then

            'Identifichiamo l'operatore
            Dim operatore As Char = Espressione(indiceOperatore)
            Debug.WriteLine("Operatore: " + operatore)
            Select Case operatore
                Case "+"
                    tipo = SOMMA
                Case "-"
                    tipo = DIFFERENZA
                Case "*"
                    tipo = PRODOTTO
                Case "/"
                    tipo = QUOZIENTE
                Case "^"
                    tipo = ESPONENZIALE
            End Select

            'un'operazione binaria è una funzione con due argomenti, l'array viene quindi inizializzato come segue
            ReDim argomenti(1)

            'gli argomenti vengono estrapolati dall'espressione e usati per la costruzione delle due funzioni
            'che faranno da argomenti all'operazione
            argomenti(0) = New Funzione(espressione.Substring(0, indiceOperatore))
            argomenti(1) = New Funzione(espressione.Substring(indiceOperatore + 1, espressione.Length - indiceOperatore - 1))
            Return

        End If

        'se l'indice operatore non ha raggiunto la fine della stringa, singifica che non ci sono operatori
        'binari l'ultima possibilità è che l'espressione sia nella forma fun(...)
        If indiceOperatore = espressione.Length - 1 Then

            'indice della parentesi aperta che contiene gli argomenti
            Dim indiceAperta As Integer = Espressione.IndexOf("(")

            'questa string è la parte "sin" in sin(x) o "log" in log(x;10)
            Dim idFunzione As String = Espressione.Substring(0, indiceAperta)

            Debug.WriteLine("idFunzione: " + idFunzione)

            'previene la creazione di funzioni con identificatori nulli
            If idFunzione = "" Then
                Return
            End If

            'Con questa istruzione inizializziamo l'array degli argomenti
            argomenti = ottieniArgomenti(espressione)

            'In base all' idFunzione si stabilisce il tipo della funzione
            'In ogni caso l'assegnazione del tipo deve avvenire dopo la verifica che il numero di argomenti 
            'sia quello aspettato
            'Per le funzioni sqrt, cbrt e ln notiamo che viene aggiunto un secondo argomento a seguito del 
            'ridimensionamento dell'array. Il secondo argomento le rende calcolabili nello stesso modo delle
            'loro controparti "generiche" (root e log)

            Select Case idFunzione

                Case "sqrt"
                    If argomenti.Length = 1 Then
                        tipo = ESPONENZIALE
                        ReDim Preserve argomenti(1)
                        argomenti(1) = New Funzione("1/2")
                    End If
                    Return

                Case "cbrt"
                    If argomenti.Length = 1 Then
                        tipo = ESPONENZIALE
                        ReDim Preserve argomenti(1)
                        argomenti(1) = New Funzione("1/3")
                    End If
                    Return

                Case "root"
                    If argomenti.Length = 2 Then
                        argomenti(1) = New Funzione("1") / argomenti(1)
                    End If
                    Return

                Case "log"
                    If argomenti.Length = 2 Then
                        tipo = LOGARITMO
                    End If
                    Return

                Case "ln"
                    If argomenti.Length = 1 Then
                        tipo = LOGARITMO
                        ReDim Preserve argomenti(1)
                        argomenti(1) = New Funzione("e")
                    End If
                    Return

                Case "abs"
                    If argomenti.Length = 1 Then
                        tipo = VALORE_ASSOLUTO
                    End If
                    Return

                Case "sin"
                    If argomenti.Length = 1 Then
                        tipo = SENO
                    End If
                    Return

                Case "cos"
                    If argomenti.Length = 1 Then
                        tipo = COSENO
                    End If
                    Return

                Case "tan"
                    If argomenti.Length = 1 Then
                        tipo = TANGENTE
                    End If
                    Return

            End Select
        End If
    End Sub

    'In base al carattere "s" in entrata risponde con la priorità dell'operatore
    'un numero più alto corrisponde a una priorità minore
    'il risultato è 0 nel caso in cui il carattere non sia un operatore
    Private Shared Function priorita(ByVal s As Char) As Integer

        Select Case s
            Case "+", "-"
                Return 3
            Case "*", "/"
                Return 2
            Case "^"
                Return 1
            Case Else
                Return 0
        End Select

    End Function

    'Questo metodo estrae gli argomenti di una funzione e li inserisce in un array sotto forma di Funzioni
    Private Shared Function ottieniArgomenti(ByRef espressione As String) As Funzione()

        'Questo primo ciclo serve per contare il numero di argomenti che è uguale al numero di separatori
        'incrementato di 1. Non dobbiamo contare i punto e virgola che si trovano dentro ad altre parentesi
        'in quanto non sono i separatori della funzione in oggetto, ma saranno con ogni probabilità separatori
        'delle funzioni che fanno da argomento ad essa es: sin(log(100;10)) il ; non separa gli argomenti 
        'della funzione seno ma quelli della funzione logaritmo, non dobbiamo considerarlo a questo livello di
        'astrazione, sarà oggetto di analisi dei costruttori delle "funzioni figlie"

        'Se abbiamo trovato la forma fun(...) significa che un argomento c'è di sicuro anche se nullo, cioè 
        'string vuota( "" )
        Dim nArgomenti As Integer = 1

        Dim aperte As Integer = 0

        Dim c As Char

        For i = 0 To espressione.Length - 1

            c = espressione(i)

            If c = "(" Then
                aperte = aperte + 1
            End If
            If c = ")" Then
                aperte = aperte - 1
            End If

            'contando solo i ; quando siamo dentro a una sola coppia di parentesi possiamo assicurarci di
            'quanto detto due commenti fa
            If aperte = 1 And c = ";" Then
                nArgomenti = nArgomenti + 1
            End If

        Next

        'Ora che sappiamo il numero di argomenti possiamo inizializzare l'array 
        Dim argomenti(nArgomenti - 1) As Funzione
        Debug.WriteLine("La funzione ha " + CStr(argomenti.Length) + " argomenti")


        'Nel ciclo seguente comporremo un argomento alla volta un carattere per volta in modo da 
        'creare l'array argomenti. 
        'questa string serve per conservare i caratteri dell'argomento corrente che sarà usato per costruire 
        'una funzione figlia al suo completamento
        Dim argomento As String = ""

        'questo contatore tiene traccia di quanti argomenti abbiamo ricavato completi finora
        Dim nArgomento As Integer = 0


        aperte = 0

        For i = 0 To espressione.Length - 1

            'Se il numero di argomenti trovati raggiunge il numero di argomenti totali prematuramente,
            'cioè prima di arrivare alla fine della string, c'è probabilmente un errore di sintassi
            'interrompiamo quindi la creazione degli argomenti
            If nArgomento > nArgomenti - 1 Then
                Exit For
            End If

            c = espressione(i)

            If c = "(" Then
                aperte = aperte + 1
            End If
            If c = ")" Then
                aperte = aperte - 1
            End If

            If (aperte = 1 And c = ";") Or (aperte = 0 And c = ")") Then
                'La stringa accumulata finora viene usata per costruire un'oggetto funzione da assegnare
                'nell'array argomenti quando si arriva a un punto e virgola o all'ultima parentesi chiusa
                argomenti(nArgomento) = New Funzione(argomento)

                'passiamo quindi all'argomento successivo e partiamo con una nuova stringa vuota
                nArgomento = nArgomento + 1
                argomento = ""


            ElseIf aperte >= 1 And Not (aperte = 1 And c = "(") Then
                'Se non stiamo osservando un simbolo separatore o la prima o ultima parentesi chiusa e ci 
                'troviamo ancora tra le parentesi che racchiudono gli argomenti (che implica aperte>=1)
                'aggiungiamo il carattere corrente all'argomento corrente
                argomento = argomento + c
            End If

        Next

        Return argomenti

    End Function

    'Risponde con vero se la stringa contiene almeno uno dei simboli contenuti nell'array simboli()
    Private Shared Function contieneSimboli(ByRef espressione As String) As Boolean

        'Appena si incontra un simbolo usciamo dalla funzione con valore "vero"
        'Se tutti i cicli si eseguono senza il riscontro di simboli allora rispondiamo "falso"
        For Each c As Char In espressione
            For Each s As Char In simboli
                If c = s Then
                    Return True
                End If
            Next s
        Next c

        Return False
    End Function

    'Prende come parametro il riferimento a una stringa che viene modificata direttamente, non c'è quindi un
    'valore restituito
    'Per eliminare gli spazi essi vengono semplicemente sostituiti con "niente"
    Private Shared Sub RimuoviSpazi(ByRef espressione As String)
        espressione = espressione.Replace(" ", "")
    End Sub

    'Conta le parentesi e verifica la congruenza di esse, restituisce valore vero se tutto va a buon fine
    'altrimenti falso
    'Inoltre elimina le parentesi esterne in eccesso (anche più di una coppia) per esempio:
    '(x+1) diventa x+1 , ((4)) diventa 4
    Private Shared Function PulisciParentesi(ByRef espressione As String) As Boolean

        'Questo controllo interrompe la funzione e restituisce valore Falso in caso che una parentesi sia
        'chiusa prima di essere aperta
        If espressione.IndexOf("(") > espressione.IndexOf(")") Then
            Return False
        End If

        'Questa variabile serva da contatore per verificare il bilanciamento delle parentesi
        Dim aperte As Integer = 0

        'L'espressione viene analizzata un carattere alla volta, il contatore sarà incrementato ogni volta che
        'troviamo una parentesi aperta e decrementato quando ne troviamo una chiusa
        For Each c As Char In espressione
            If c = "(" Then
                aperte = aperte + 1
            End If
            If c = ")" Then
                aperte = aperte - 1
            End If
        Next

        If aperte = 0 Then
            'Se alla fine della stringa il contatore è uguale a zero significa che parentesi aperte e chiuse 
            'esistono in numero uguale. Procediamo quindi a cercare ed eventualmente eliminare le parentesi in
            'eccesso. Se il primo carattere è ( e l'ultimo è ) posso eliminarle solo dopo aver verificato che 
            'esse sono corrispondenti tra loro. Infatti in (x^2+2x+1) posso eliminare le parentesi esterne
            'mentre farlo nella funzione (x+1)*(x-2) la renderebbe invalida.
            'Per la verifica della prima condizione impiego un ciclo in modo da ripetere la rimozione nel caso
            'di più di una coppia di parentesi in eccesso.
            While Left(espressione, 1) = "(" And Right(espressione, 1) = ")"

                ' "La prima e l'ultima parentesi devono essere corrispondenti" è qui interpretato meglio come:
                ' "L'indice della parentesi chiusa corrispondente alla prima deve essere uguale all'indice 
                '  dell'ultimo carattere della stringa"
                If indiceChiusa(espressione) = espressione.Length - 1 Then

                    'Per rimuovere primo e ultimo carattere uso la funzione substring
                    espressione = espressione.Substring(1, espressione.Length - 2)
                    'A seguito della rimozione si ritorna al controllo del ciclo While

                Else
                    'Se le parentesi più esterne non sono corrispondenti esco dal ciclo while
                    Exit While
                End If
            End While

            'Arriviamo qui certi che le parentesi sono bilanciate e che non ce ne sono in eccesso, possiamo
            'quindi restituire Vero
            Return True
        Else

            'Se il contatore resta diverso da 0 significa che le parentesi sono sbilanciate
            Return False

        End If
    End Function

    'Restituisce l'indice della parentesi chiusa corrispondente alla prima parentesi aperta che deve essere ad
    'indice 0
    Private Shared Function indiceChiusa(ByRef espressione As String) As Integer

        'indice del carattere osservato, parte da 1 perchè saltiamo il primo carattere (di indice 0)
        Dim indice As Integer = 1
        'tiene traccia delle parentesi aperte, parte da 1 perchè sappiamo già che il primo carattere è una (
        Dim aperte As Integer = 1

        'Appena ci troviamo al di fuori delle parentesi, usciamo dal ciclo, l'indice con cui usciamo del ciclo
        'è quello della parentesi chiusa che cerchiamo incrementato di 1, viene quindi restituito dopo essere
        'decrementato di 1
        While Not aperte = 0
            If espressione(indice) = "(" Then
                aperte = aperte + 1
            End If
            If espressione(indice) = ")" Then
                aperte = aperte - 1
            End If

            indice = indice + 1

        End While

        Return indice - 1

    End Function

    'Questa procedura viene eseguita per trasformare le funzioni create come esponenziali nella forma più
    'semplice di potenze
    Private Sub filtraPotenze()
        If argomenti(1).tipo = COSTANTE Then
            tipo = POTENZA
        End If
    End Sub

    'Costanti definite per i possibili valori assunti dall'attributo "tipo"
    Private Const INVALIDA As Integer = 0
    Private Const COSTANTE As Integer = 1
    Private Const X As Integer = 2
    Private Const SOMMA As Integer = 3
    Private Const DIFFERENZA As Integer = 4
    Private Const PRODOTTO As Integer = 5
    Private Const QUOZIENTE As Integer = 6
    Private Const POTENZA As Integer = 7
    Private Const ESPONENZIALE As Integer = 8
    Private Const LOGARITMO As Integer = 9
    Private Const VALORE_ASSOLUTO As Integer = 10
    Private Const SENO As Integer = 11
    Private Const COSENO As Integer = 12
    Private Const TANGENTE As Integer = 13

    'Array contenente i simboli che non devono essere presenti perchè una funzione sia costante
    Private Shared simboli As Char() = {"+", "-", "*", "/", "^", "(", ")"}

    Private Sub calcolaCostante()
        If tipo = X Or tipo = COSTANTE Then
            Return
        End If
        For Each argomento As Funzione In argomenti
            If Not argomento.tipo = COSTANTE Then
                Return
            End If
        Next
        valore = Calcola(0)
        tipo = COSTANTE
        ReDim argomenti(-1)
        Debug.WriteLine("Sostituita costante")
    End Sub




End Class
