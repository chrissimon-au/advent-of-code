Class {
	#name : #Position,
	#superclass : #Object,
	#instVars : [
		'col',
		'row'
	],
	#category : #'AoC-Day16-Model'
}

{ #category : #accessing }
Position >> col: anInteger [ 
	col := anInteger
]

{ #category : #accessing }
Position >> row: anInteger [ 
	row := anInteger
]
