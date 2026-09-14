package geometria

/**
 * Operaciones sobre triángulos en el plano.
 *
 * Un triángulo se describe con las coordenadas de sus tres vértices:
 * (ax, ay), (bx, by) y (cx, cy).
 *
 * Las cuatro funciones están sin implementar: el cuerpo dice ??? , que en
 * Scala significa justamente eso y hace fallar la prueba al ejecutarse.
 * Reemplace cada ??? por su solución.
 */
class Geometria {

  /**
   * Punto 1: distancia entre dos puntos del plano.
   *
   * Recuerde:  d = raiz cuadrada de (x2-x1)^2 + (y2-y1)^2
   * En Scala la raíz cuadrada es math.sqrt(...)
   *
   * distancia(0, 0, 3, 4) == 5.0
   * distancia(1, 1, 1, 1) == 0.0
   */
  def distancia(x1: Double, y1: Double, x2: Double, y2: Double): Double = {
    math.sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
  }

  /**
   * Punto 2: perímetro del triángulo.
   *
   * Es la suma de los tres lados. No vuelva a escribir la fórmula de la
   * distancia: llame a la función del punto 1.
   *
   * perimetro(0, 0, 3, 0, 0, 4) == 12.0
   */
  def perimetro(ax: Double, ay: Double,
                bx: Double, by: Double,
                cx: Double, cy: Double): Double = {
    distancia(ax, ay, bx, by) +
      distancia (bx, by, cx, cy) +
      distancia (cx, cy, ax, ay)
  }

  /**
   * Punto 3: área del triángulo por la fórmula de Herón.
   *
   * Con s el semiperímetro, es decir la mitad del perímetro:
   *
   *     area = raiz cuadrada de  s (s - ab) (s - bc) (s - ca)
   *
   * area(0, 0, 3, 0, 0, 4) == 6.0
   *
   * Si los tres puntos están sobre una misma recta no forman un triángulo.
   * En ese caso el producto de adentro de la raíz da cero, y la función debe
   * fallar con IllegalArgumentException en lugar de retornar un número.
   * Para eso sirve require(condicion, "mensaje"): lanza esa excepción cuando
   * la condición es falsa.
   */
  def area(ax: Double, ay: Double,
           bx: Double, by: Double,
           cx: Double, cy: Double): Double = {
    val ab = distancia(ax, ay, bx, by)
    val bc = distancia(bx,by, cx, cy)
    val ca = distancia(cx, cy, ax, ay)

    val s = (ab + bc + ca) / 2

    require(s > 0 && (s - ab) > 0 && (s - bc) > 0 && (s - ca) > 0,
      "Los puntos no forman un triangulo")

    math.sqrt(s * (s - ab) * (s - bc) * (s - ca))
  }

  /**
   * Punto 4: clasificación del triángulo según sus lados.
   *
   * Retorna exactamente una de estas tres palabras:
   *   "equilatero"  si los tres lados miden lo mismo
   *   "isosceles"   si exactamente dos lados miden lo mismo
   *   "escaleno"    si los tres lados son distintos
   *
   * Cuidado: dos lados calculados con raíces cuadradas casi nunca dan
   * exactamente iguales. Compare la diferencia contra una tolerancia
   * pequeña en lugar de usar ==.
   *
   * clasificar(0, 0, 3, 0, 0, 4) == "escaleno"
   */
  def clasificar(ax: Double, ay: Double,
                 bx: Double, by: Double,
                 cx: Double, cy: Double): String = {
    val ab = distancia (ax, ay, bx, by)
    val bc = distancia (bx, by, cx, cy)
    val ca = distancia(cx, cy, ax, ay)

    val tolerancia = 1e-9

    val abIgualBc = math.abs(ab - bc) < tolerancia
    val bcIgualCa = math.abs(bc - ca) < tolerancia
    val abIgualCa = math.abs(ab - ca) < tolerancia

    if (abIgualBc && bcIgualCa){
      "equilatero"
    }else if (abIgualBc || bcIgualCa || abIgualCa){
      "isosceles"
    } else {
      "escaleno"
    }
  }
}
