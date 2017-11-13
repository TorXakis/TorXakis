# Assignment - Lucky People

Your assignment is to test the Lucky People [program][8] with [TorXakis][7].

## Requirements

The Lucky People [program][8] gets as input a sequence of persons on socket 7777.
A person has a sex, first name, last name, day and month of birth.
For each person, the Lucky People program produces one output, a string on socket 7777.
The string is either True or False, and reflects whether the person is considered lucky.
A person is lucky based on

*   **Name:** The first character of the first name is equal to the first character of the last name, or
*   **Birth date:** The day of birth is equal to the month of birth, or
*   **Gender:** The person is a Female after five Males or a Male after five Females.

The input sequence is specified as follows:

| Specification | Structure | Constraints |
|-|-|-|
| persons | (person '\n')* ||
| person | sex separator firstName separator lastName separator dayOfBirth separator monthOfBirth ||
| separator | '@' ||
| sex | 'Male'\|'Female' ||
| firstName | name ||
| lastName | name ||
| name | [A-Z][a-z]* ||
| dayOfBirth | Int | 1 <= dayOfBirth <= 31 |
| monthOfBirth | Int | 1 <= monthOfBirth <= 12 |
## Examples

### Lucky based on names

*   Male@Mickey@Mouse@13@1
*   Male@Donald@Duck@13@3
*   Male@Luuk@Laar@24@12

### Lucky based on birthday

*   Female@Shakira@Ripoll@2@2
*   Male@Michael@Buble@9@9
*   Female@Imke@Laar@7@7

### Lucky based on sequence

In the following sequences of persons, the persons in italics are considered lucky due to their position in the sequence

*   Male@Huey@Duck@17@10  
    Male@Dewey@Duck@17@10  
    Male@Louie@Duck@17@10  
    Male@Mickey@Mouse@13@1  
    Male@Donald@Duck@13@3  
    _Female@April@Duck@15@5_

*   Female@Beatrix@Oranje@31@1  
    Female@Maxima@Zorreguieta@17@5  
    Female@Amalia@Oranje@7@12  
    Female@Alexia@Oranje@26@6  
    Female@Ariane@Oranje@10@4  
    _Male@Willem@Oranje@27@4_

## Deliverables

Provide a TorXakis model: a file named 'LuckyPeople.txs' containing a [model definition][5] and [connection definition][6].
Provide a test report: a file containing the verdict whether 'LuckyPeople.java' satisfies the requirements and the actual trace(s) used to test the program.


## Solution

You can find a solution [here](Modelling-Example-Lucky-People-(Solution).md).

[1]: https://github.com/TorXakis/TorXakis/wiki/TypeDefs
[2]: https://github.com/TorXakis/TorXakis/wiki/ChanDefs
[3]: https://github.com/TorXakis/TorXakis/wiki/FuncDefs
[4]: https://github.com/TorXakis/TorXakis/wiki/ProcDefs
[5]: https://github.com/TorXakis/TorXakis/wiki/ModelDefs
[6]: https://github.com/TorXakis/TorXakis/wiki/CnectDefs
[7]: https://github.com/TorXakis/TorXakis/wiki/TorXakis
[8]: https://github.com/TorXakis/TorXakis/wiki/Java_program
