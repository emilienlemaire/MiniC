# MiniC - Emilien Lemaire
* [Usage](#usage)
  + [Modules requis](#modules-requis)
  + [Compilation](#compilation)
  + [Exécution](#exécution)
* [Notes](#notes)
* [Extensions](#extensions)
  + [Extensions réalisées](#extensions-réalisées)
  + [Implementation des extensions](#implementation-des-extensions)
    - [Ajouts d'opérateurs](#op)
    - [Structures](#structures)
    - [Pointeurs](#pointeurs)
    - [Erreurs de syntaxe verbeuses](#erreurs-de-syntaxe-verbeuses)
    - [Afficheur](#afficheur)
  + [Interprète](#interprète)
    - [Mémoire](#mémoire)
    - [Variables non initialisées](#variables-non-initialisées)
    - [Structs dans la mémoire](#structs-dans-la-mémoire)
    - [Typage dans l'interprète](#inter)
    - [Exception Unreachable](#exception-unreachable)
    
## Usage

### Modules requis

* Core
* MenhirLib

### Compilation

```bash
dune build
```

### Exécution

```bash
# N.B.: Dune crée un fichier .exe sur toutes plateforme.
dune exec ./minic.exe tests/minic_example.c
# ou pour afficher le programme
dune exec -- ./minic.exe -display tests/minic_example.c
```

## Notes
J'ai commenté mon code aux endroits où cela me semblait important. Les commentaires viennent en
extensions à ce README.

Tous les fichiers utilisés pour tester ce programme sont dans le dossier `tests`, s'ils échouent
leur nom contient le mot `error`.

## Extensions
### Extensions réalisées
 - [x] Ajouts d'opérateur
 - [x] Structures
 - [x] Pointeurs
 - [x] Erreurs syntaxiques verbeuses
 - [x] Afficheur
 - [X] Interprète
 
### Implementation des extensions
#### <a name=op></a>Ajout d'opérateurs
J'ai ajouté les opérateurs binaires suivants: `== != - / * % <= >= > && ||` et les opérateurs unaires
suivants: `- ! &`.

Dans le vérificateur de types, `- / * <= >= >` s'attendent à recevoir des expressions de type `int`,
un erreur apprait sinon.

Les autres opérateurs acceptent tout type d'expressions.

#### Structures
Pour l'ajout des structures je n'autorise pas l'utilisateur à créer des structures anonymes, mais
dans mon vérificateur de type et mon interprète, les listes d'initialisation (`{1, 2, 3}`) sont vues
commes des structures anonymes.

La syntaxe choisi pour définir, déclarer, modifier, ou accéder à une structure est présentée ci-dessous:
```c
struct myStrcyt {
  int a;
  int b;
}

struct mySecondStruct {
  int a;
  struct myStruct s;
}

// Déclaration sans initialisation autorisé, l'interprète initialisera
// les membres à leurs valeurs par défaut, explicité dans le paragraphe
// interprète de ce README.
// Toutes les déclarations de varaibles/fonctions de type struct doivent
// commencer par le mot clef struct.
struct myStruct s;

// Initialisation avec une liste d'initialisation,
// le seul type d'initialisation de structure implémenté
struct myStruct ms = {1, 2};

// On peut aussi mettre une liste d'initialisation dans une autre si
// un des membres de la structure est un type.
struct mySecondStruct mss = {1, {2, 3}};

// Les fonctions peuvent retourner un type struct et prendre en argument
// un type struct.
// L'expression de retour peut être une variable struct instantiée ou une
// liste d'initialisation.
struct myStruct addToBoth(struct myStruct s, int i) {
  if (i > 0) {
    struct myStruct s2 = s;
    s2.a = s2.a + i;
    s2.b = s2.b + i;
    
    return s2;
  } else {
    
    return {s.a + i, s.b + i};
  }
}
```

#### Pointeurs

Les pointeurs ont été ajouté dans mon MiniC. Toutes les opérations sur les pointeurs sont explicités
dans le code ci-dessous.
```c
// Ceci créer un pointeur vers une valeur NULL, mais on peut condérer
// que la mémoire est allouée dans l'interprète après cette opération,
// pas de malloc nécessaire. Aussi cela signifie qu'il n'y a pas de
// free, j'ai donc décidé d'ajouter un garbage collector qui supprime
// les variables aux adresses qui n'ont pas de référence dans le reste
// de la mémoire après chaque appelle de fonction.
int* PTR;

// Assigner l'adresse d'une variable à un pointeur.
int PARAM = 5;
int *PTR2 = &PARAM;

// Pointeur de pointeur, un niveau infini de référence est autorisé.
int **PTR3;

// Pointeur de structure et déréferencement.
struct myStruct *s = {*PTR2, PARAM};

// Accès à un membre d'un pointeur de structure.
int i = s->b;

// Plusieurs opérations sur les pointeurs à la suite
int j = *(s->a);

// Modification de la mémoire avec un offset.
// ATTENTION: Cette opération est autorisée, mais au mieux modifiera
// une variable sans importance, au pire lèvera une exception de l'interprète
// pour accès à une case mémoire non allouée, elle peut aussi modifier une
// variable quelconque importante.
*(PTR2 + 1) = 12;
```
#### Erreurs de syntaxe verbeuses
Grâce a l'API incrémentale de Menhir j'ai pu faire des erreurs de syntaxes
assez précises. Vous pouvez voir ces erreurs dans le fichier `minicparser.messages`,
il est assez long mais quasiment toutes les erreurs de syntaxe possible avec
ma grammaire sont répertoriés.

#### Afficheur
J'ai réalisé l'afficheur, tout le code de celui-ci est implémenté dans `minicPrinter.ml`.
Il affiche le code à partir de l'AST après que celui-ci soit passé dans le vérificateur de type
et qui sera envoyé dans l'interpréteur.
Vous pouvez ajouter le drapeau `-display` au programme afin qu'il affiche le fichier que vous
voulez interpréter.

### Interprète

J'ai réalisé l'interprète Minic. Je donne ici quelques détails d'implémentation.

#### Mémoire
Tout d'abord la mémoire est composée de deux tables de hashage, une première qui
enregistre le nom des variables et l'associe à une adresse (appelé `memory_map` dans mon code),
une seconde qui a les adresses et leur associe une valeur (appelée simplement `memory`). \
Cette implémentation permet de mieux manipuler les pointeurs dans l'interprète. <br/>
Par example après l'initialisation des variables globales et l'ajout des variables locales de `main`
du fichier `minc_example.c`, la mémoire de l'interprète ressemble à ceci:

<table>
<tr><th>memory_map</th><th>memory</th>
<tr><td>

|Nom            | Adresse | 
| -             | -       |
| a             | @25     |
| PARAM         | @3      |
| example       | @15     |
| v             | @24     |
| strPtr        | @20     |
| PTR2          | @7      |
| i             | @11     |
| b1            | @9      |
| structExample |  @12    |
| nonInitilized |  @0     |
| PTR           |  @4     |

</td><td>

| Adresse |  Valeur                       |    
| ------- | ----------------------------- |
|0        | Struct(a: @1, b: @2)          |
|1        | Int(0)                        |
|2        | Int(0)                        |
|3        | Int(5)                        |
|4        | Prt(@5)                       |
|5        | Prt(@6)                       |
|6        | Null                          |
|7        | Prt(@3)                       |
|9        | Struct(a: @10)                |
|10       | Int(5)                        |
|11       | Int(0)                        |
|12       | Struct(a: @13, b: @14)        |
|13       | Int(0)                        |
|14       | Int(0)                        |
|15       | Struct(theStr: @16, b: @19)   |
|16       | Struct(a: @17, b: @18)        |
|17       | Int(0)                        |
|18       | Int(0)                        |
|19       | Int(0)                        |
|20       | Prt(@21)                      |
|21       | Struct(a: @22, b: @23)        |
|22       | Int(0)                        |
|23       | Int(0)                        |
|24       | Int(0)                        |
|25       | Struct(a: @26, b: @29)        |
|26       | Prt(@27)                      |
|27       | Prt(@28)                      |
|28       | Null                          |
|29       | Prt(@30)                      |
|30       | Null                          |

</td></tr>
</table>

#### Variables non initialisées
Comme vous pouvez le voir dans la table ci-dessus, les variables non initialisées prennent une valeur.
Pour les `int` c'est **0**, pour les bool c'est **0** aussi (soit *false*). Pour les pointeurs la valeur
par défaut est `Null`. Pour les structs chaque membre reçoit la valeur par défaut de son type.

#### Structs dans la mémoire
Comme vous pouvez le voir dans la table, et afin de ce rapprocher du C, les membres des structs
pointent vers une case mémoire, souvent la suivante.

#### <a name=inter></a>Typage dans l'interprète
Pour l'interprète j'ai créer de nouveaux types afin de mieux pouvoir les manipuler pour l'interprétation.
Le plupart ne changent pas mais, j'ai ajouté un type `NamedInitList` qui permet, lorsque les fonctions
retournent une struct de nommé la liste d'initialisation retournée et d'accéder directement aux membres
de cette dernière (cf. `tests/minic_example.c:82`).

#### Exception Unreachable
Dans mon interpréteur j'ai une exception `Unreachable`, celle-ci est présente car ces parties de
code ne devraient pas s'exécuter car le vérificateur de type vérifie déjà ces parties de code innacessible
à l'interpréteur.
