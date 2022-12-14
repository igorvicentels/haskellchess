Esta é uma versão do xadrez implementada em Haskell.

## Arquivos presentes neste repositório

- `Main.hs`: inicializa o jogo;
- `Types.hs`: definição dos tipos;
- `Board`: apresenta as funções relacionadas aos movimentos das peças; 
- `Endgame.hs`: apresenta as funções de verificação das situações de finalização do jogo;
- `IO.hs`: funções de input e output;

## Como compilar e executar o jogo
Primeiramente é preciso compilar os arquivos. Para isso, utilize o comando:

```sh
ghc Main
```
Após compilado, basta utilizar o seguinte comando para executar:

```sh
./Main
```

## Como jogar
Para movimentar uma peça é preciso digitar as coordenadas de origem e as coordenadas de destino. Por exemplo, caso queira movimentar uma peça da casa `E2` para a casa `E4`, basta digitar `e2e4`.
Caso o movimento seja de promoção do peão, além de indicar as coordenadas de origem e de destino, o jogador deve indicar também qual a peça escolhida. Por exemplo, caso o jogador de brancas deseje obter uma rainha ao realizar uma promoção do peão que irá se movimentar da casa `A7`para a casa `A8`, basta digitar `a7a8 qw`.
As possíveis escolhas de peças na promoção do peão são:

-`qw`: rainha branca       -`qb`: rainha preta

-`rw`: torre branca        -`rb`: torre preta

-`bw`: bispo branco        -`bb`: bispo preto

-`nw`: cavalo branco       -`nb`: cavalo preto
