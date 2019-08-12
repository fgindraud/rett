Rett - Relation Editor and Tracking Tool
========================================

Model any data using a corpus of structured and referenceable sentences.

**This project is now stopped, replaced by Wimd for a 3rd iteration on the RPG-specific-wiki concept**

## Pros ##
The data model is usable in practice.
The ability to directly use sentences (relations) as subjects is very powerful and allows to compose complex sentences (adverb, annotations, etc).

The structure enforced on data automatically group data related to an element in its graph neighbourhood, which makes data finding easy.
A more complex query system is however needed to combine multiple source of information.

The model should allow to annotate data by its "known to players" status for a RPG Game Master. Not tested though.

## Cons ##
Data input is very slow in the wiki interface.
It could be improved by a complex autocompletion system with elements as text, but this require that the database level knows the prefered text representation.

This solution is however problematic, as the model does not distinguish between a sentence with adverb and an external annotation.
In both case this is represented as a sentence in subject of another one.
Thus the text representation of the first sentence is ambiguous, which is a problem for displaying sentences in general.
Solving this means having a more general modelisation of sentences which starts to go outside the scope of a hobby project.

## Planed/possible extensions to the current state ##
- Fast data entry: parse from text, and/or editing with autocompletion.
- Structural queries with a query language. Ex: "find name of elements in group 'bad guy' seen in city 'some place'".
- Displaying the database as an explorable interactive graph. Requires complex layout system, as this is not a standard graph representation.
