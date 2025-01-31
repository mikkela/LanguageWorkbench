package org.mikadocs.language.workbench

trait Value
class StringValue extends Value
class IntValue extends Value

// Unit tests
class RegistryTest extends munit.FunSuite {

  test("Registry should allow registering and retrieving a value") {
    val registry = new Registry[Class[_ <: Value], Value]()
    val stringValue = new StringValue

    registry.register(classOf[StringValue], stringValue)

    assertEquals(registry.get(classOf[StringValue]), Some(stringValue))
  }

  test("Registry should return None for an unregistered key") {
    val registry = new Registry[Class[_ <: Value], Value]()

    assertEquals(registry.get(classOf[StringValue]), None)
  }

  test("Registry should allow multiple registrations") {
    val registry = new Registry[Class[_ <: Value], Value]()
    val stringValue = new StringValue
    val intValue = new IntValue

    registry.register(classOf[StringValue], stringValue)
    registry.register(classOf[IntValue], intValue)

    assertEquals(registry.get(classOf[StringValue]), Some(stringValue))
    assertEquals(registry.get(classOf[IntValue]), Some(intValue))
  }

  test("Registry should overwrite a value for the same key") {
    val registry = new Registry[Class[_ <: Value], Value]()
    val firstValue = new StringValue
    val secondValue = new StringValue

    registry.register(classOf[StringValue], firstValue)
    registry.register(classOf[StringValue], secondValue)

    assertEquals(registry.get(classOf[StringValue]), Some(secondValue))
  }

  test("Registry should report the correct size") {
    val registry = new Registry[Class[_ <: Value], Value]()
    assertEquals(registry.size, 0)

    registry.register(classOf[StringValue], new StringValue)
    assertEquals(registry.size, 1)

    registry.register(classOf[IntValue], new IntValue)
    assertEquals(registry.size, 2)

    registry.register(classOf[StringValue], new StringValue) // Overwrite
    assertEquals(registry.size, 2)
  }
}
