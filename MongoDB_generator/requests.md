## Compte rendu mongoDB

### 1. Generation de fichier de donnees .json
Afin de populer la base de donnee mongoDB, nous avons choisi de generer des fichier .json grace au moyen de R. Le contenu de la base se compose d’une premiere collection d’individus, dont les carracteristiques suivantes seront contenues : prenom, age, sexe, ville de naissance, lieu d’habitation actuel, liste de films favoris, sports pratiques, ainsi qu’une rubrique “enfants”, contenant le prenom, l’age et le sexe du ou des enfants. L’exemple typique d’un individu est le suivant :


```json
{
    "name": "Victor",
    "caracteristiques": {
      "age": 41,
      "sexe": "M",
      "lieu_de_naissance": "Saint-Denis",
      "lieu_de_residence": "Nantes",
      "sport": ["Golf", "Equitation"],
      "film": "Le trou",
      "hobbie": "Sculpture"
    },
    "children": [
      {
        "prenom": "Guillaume",
        "sexe": "M",
        "age": 22
      },
      {
        "prenom": "Hugo",
        "sexe": "M",
        "age": 22
      }
    ]
  },
```

Grace au script R il a ete possible de generer aleatoirement 20.000 individus.
Les trois autres collections permettent d’apporter plus d’informations aux proprietes suivantes...

1. Villes, avec la population et le code postal.

```json
{
    "ville_fr": "Toulouse",
    "nbr_habitant": 400000,
    "code_postal": 31000
  },
```

2. sports, avec le nombre d’aherents ainsi que le pourcentage de femme pratiquant le sport.

```json
{
    "sport": "Tennis",
    "adherents": 1018721,
    "pourentageF": 45
  },
```
3. films, avec le titre, la note, l’appreciation globale et le classement

```json
{
    "id": "000004",
    "film": "Le bon, la brute et le truand",
    "caracteristiques": {
      "note": 8.4,
      "appreciation": "TB",
      "classement": 4
    }
  },
```

## Import des donnees
Lancement d’un serveur mongoDB en localhost :
```
mongod --shardsvr --dbpath mongodb --port 27021 &
mongo localhost:27021
```


Ces differents fichiers .json sont importes dans une base appellee mongotp (cree grace a la commande use mongotp), au moyen des commandes suivantes:
```
mongoimport --db mongotp --collection personnes --drop --file personnes.json --port 27021  --jsonArray

mongoimport --db mongotp --collection sports --drop --file sport.json --port 27021  --jsonArray

mongoimport --db mongotp --collection films --drop --file films.json --port 27021  --jsonArray

mongoimport --db mongotp --collection villes --drop --file villes.json --port 27021  --jsonArray
```

## Creation des index

La creation d'index appropries va permettre une acceleration des recherches dans la base de donnee.

```
db.personnes.createIndex({name: 1})
db.personnes.createIndex({caracteristique.age: -1})
```

## Requetes effectuees:

1. Compter le nombre de femmes nees a Paris, vivant a grenoble ayant au moins 2 enfants et ayant au moins 40 ans.

```
db.personnes.aggregate(

  {$match:{"caracteristiques.sexe":"F","caracteristiques.lieu_de_residence" :"Grenoble","caracteristiques.lieu_de_naissance" :"Paris", "caracteristiques.age" : {$gt :40}}},
    { $unwind: "$children" },
    { $group: {
        _id: "$_id",
        nbenfant: { $sum: 1 }
    }},
{$match: {nbenfant : {$gte : 2}}},
{$count : "Result"}
)
```
Commentaire: Pas d'evocation d'index dans cette requete,


2. Compter le nombre de d’enfants total references dans la base de donnes qui ont ont pour parent des hommes de plus de 55 ans et qui ont deux passions.

```
db.personnes.aggregate(

  {$match:{"caracteristiques.sexe":"M", "caracteristiques.age" : {$gt :55},  "caracteristiques.hobbie" : { $size: 2 } }},
	{ $unwind: "$children" },
	{ $group: { _id: null, count: { $sum: 1 }}}
)
```

3. Lister le nombre de femmes qui effectuent des sports dont la representation feminine est superieur a 25 % et qui vivent dans des villes de plus de 200000 habitants.

```
db.personnes.aggregate([
	{ $lookup: {
		from: "sports",
		localField: "caracteristiques.sport",
		foreignField: "sport",
		as: "sport_detail"
	} },
		{ $lookup: {
		from: "villes",
		localField: "caracteristiques.lieu_de_residence",
		foreignField: "ville_fr",
		as: "ville_detail"
	} },
  {$match:{"caracteristiques.sexe":"F", "ville_detail.nbr_habitant" : {$gt :200000},"sport_detail.pourentageF" : {$gt :25}}},
{ $group: { _id: null, count: { $sum: 1 }}}
])
```

4.Lister les parents qui ont deux enfants, qui ont pour film favori un film dont la note est superieure a 5.5 et qui sont nes dans une ville de moins de 180 000 habitants.

```
db.personnes.aggregate(
	{ $lookup: {
		from: "films",
		localField: "caracteristiques.film",
		foreignField: "film",
		as: "film_detail"
	} },
		{ $lookup: {
		from: "villes",
		localField: "caracteristiques.lieu_de_naissance",
		foreignField: "ville_fr",
		as: "ville_detail"
	} },
{$match:{"ville_detail.nbr_habitant" : {$lt :180000},"film_detail.caracteristiques.note" : {$gt :5.5}}},
{ $unwind: "$children" },
{ $group: { _id: "$_id", count: { $sum: 1 }}},
{$match:{"count" : 2}}
)
```

5. rouver les individus ayant au moins de 56 ans,pratiquant le tennis et ayant pour film favori Point limite, les classer par age descendant puis par ordre alphabetique de prenom, limiter la recherche a 50 individus.

```
db.personnes.find({"caracteristiques.age" : {$gt :56},"caracteristiques.sport":"Tennis"}).sort({"caracteristiques.age" :-1,name:1}).limit(50).pretty()
```
