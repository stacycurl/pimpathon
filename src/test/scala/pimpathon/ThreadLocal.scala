package pimpathon


class ThreadLocalSpec extends PSpec {
  "create" in threadLocal.create(1).get() ≡ 1
}