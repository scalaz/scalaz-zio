package zio.examples.bank.service

import zio.examples.bank.TestEnvironment._
import zio.examples.bank.domain.{ Account, CreateAccount }
import zio.examples.bank.environment.Environments.BankEnvironment
import zio.examples.bank.failure.{ AccountFailure, AccountNotFound }
import zio.examples.bank.service.AccountServiceImpl._
import zio.test.Assertion._
import zio.test._

object AccountServiceSpec
    extends DefaultRunnableSpec(
      {
        suite("AccountServiceSpec")(
          testM("Create account") {

            val command = CreateAccount("John Doe")

            val pipeline = createAccount(command).either

            val assertion =
              assertM[BankEnvironment, Nothing, Either[AccountFailure, Account]](
                pipeline,
                isRight(equalTo(Account(1, "John Doe")))
              )

            testEnv >>= assertion.provide

          },
          testM("Find an existent account") {

            val command = CreateAccount("John Doe")

            val pipeline = for {
              a   <- createAccount(command)
              res <- findAccountById(a.id)
            } yield res

            val assertion =
              assertM[BankEnvironment, Nothing, Either[AccountFailure, Account]](
                pipeline.either,
                isRight(equalTo(Account(1, "John Doe")))
              )

            testEnv >>= assertion.provide

          },
          testM("Fail to find a nonexistent account") {
            val id       = 1
            val pipeline = findAccountById(id).either

            val assertion = assertM[BankEnvironment, Nothing, Either[AccountFailure, Account]](
              pipeline,
              isLeft(equalTo(AccountNotFound(id)))
            )

            testEnv >>= assertion.provide

          }
        )
      }
    )
