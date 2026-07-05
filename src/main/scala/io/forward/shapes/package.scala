package io.forward

package object shapes {
  type Id[A] = A

  object Id {
    def apply[A](value: A): Id[A] = value
  }
}
